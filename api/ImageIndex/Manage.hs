-- | Provides primitives to manage the in-memory transactional index.
module ImageIndex.Manage (
    -- * Index creation
      newIndex
    -- * Index transactions
    , runTransaction, liftSTM
    -- * User index management
    , getUserIndex, removeUserIndex, userIndexSize, touchUserIndex
    -- * Tag management
    , getTag, lookupTag, removeTag, removeTagIfOrphan, getTagImages
    -- * Images management
    , imageCodeLength, newImageCode
    , addImage, newImage, lookupImage, removeImage, bindImage, unBindImage
    , bindImageTag, unBindImageTag, getImages
    )
    where

import Prelude

import Control.Applicative
import Control.Concurrent.STM (
      atomically, modifyTVar', newTVar, newTVarIO, readTVar, writeTVar
    )
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (STM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.SHA (hmacSha1, integerDigest)
import Data.Digits (digits)
import qualified Data.Foldable as F
import Data.Int
import qualified Data.Map as M
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import qualified Data.Vector.Storable as V
import System.Random (RandomGen, random)

import ImageIndex.Type

newIndex :: MonadIO m => m ImageIndex
newIndex = liftIO $ ImageIndex <$> newTVarIO M.empty <*> newTVarIO Nothing
                               <*> newTVarIO Nothing

-- Index transactions ----------------------------------------------------------

runTransaction :: MonadIO m => IndexSTM a -> m a
runTransaction action = liftIO $ do
    time <- getCurrentTime

    atomically $ runReaderT action time

liftSTM :: STM a -> IndexSTM a
liftSTM = lift

-- Users -----------------------------------------------------------------------

-- | Searches for an existing user index entry by the user name and returns it.
--
-- If a such entry doesn\'t exist, creates a new one.
getUserIndex :: ImageIndex -> UserName -> IndexSTM UserIndex
getUserIndex ii@(ImageIndex {..}) username = do
    time        <- ask
    iiUsersVal  <- lift $ readTVar iiUsers
    case M.lookup username iiUsersVal of
        Just userIdx -> return userIdx
        Nothing      -> do
            userIdx <- attachNode ii (newUserIndex time)
            lift $ writeTVar iiUsers (M.insert username userIdx iiUsersVal)

            return userIdx
  where
    newUserIndex time prev next = lift $ do
        rootTag <- Tag RootTag <$> newTVar M.empty <*> newTVar S.empty
        UserIndex username <$> newTVar M.empty <*> pure rootTag
                           <*> newTVar time    <*> newTVar prev
                           <*> newTVar next

removeUserIndex :: ImageIndex -> UserIndex -> IndexSTM ()
removeUserIndex ii@(ImageIndex {..}) ui@(UserIndex {..}) = do
    lift $ modifyTVar' iiUsers (M.delete uiName)
    detatchNode ii ui

userIndexSize :: UserIndex -> IndexSTM Int
userIndexSize UserIndex {..} = M.size <$> lift (readTVar uiImages)

-- Tags ------------------------------------------------------------------------

-- | Returns the last tag of the requested hierarchy of tags.
-- Creates tags which don\'t exist on the path.
getTag :: UserIndex -> TagPath -> IndexSTM Tag
getTag UserIndex {..} (TagPath path) = do
    lift $ dfs path uiRootTag
  where
    dfs []       tag               = return tag
    dfs (t : ts) parent@(Tag {..}) = do
        subTagsVal <- readTVar tSubTags
        tag <- case M.lookup t subTagsVal of
            Just tag -> return tag
            Nothing  -> do -- Sub tag doesn't exist.
                tag <- Tag (SubTag t parent) <$> newTVar M.empty
                                             <*> newTVar S.empty
                writeTVar tSubTags (M.insert t tag subTagsVal)
                return tag
        dfs ts tag

-- | Returns the last tag of the requested hierarchy of tags if the whole
-- hierarchy exists.
lookupTag :: UserIndex -> TagPath -> IndexSTM (Maybe Tag)
lookupTag UserIndex {..} (TagPath path) = do
    lift $ dfs path uiRootTag
  where
    dfs []       tag      = return $ Just tag
    dfs (t : ts) Tag {..} = do
        subTagsVal <- readTVar tSubTags
        case M.lookup t subTagsVal of
            Just tag -> dfs ts tag
            Nothing  -> return Nothing

-- | Removes the tag and its subtags. Dereferences their images. Removes the
-- parent tag if it has become orphan. Does nothing for the root tag.
removeTag :: UserIndex -> Tag -> IndexSTM ()
removeTag _  (Tag RootTag              _ _)     = return ()
removeTag ui tag@(Tag (SubTag name parent) _ _) = do
    dfs tag
    lift $ modifyTVar' (tSubTags parent) (M.delete name)
    removeTagIfOrphan parent
  where
    dfs tag'@(Tag {..}) = do
        lift (readTVar tSubTags) >>= (F.mapM_ dfs)

        imgs <- lift $ readTVar tImages
        F.forM_ imgs $ \img -> do
            unBindImage ui img
            bindImage ui img { iiTags = S.delete tag' (iiTags img) }

-- | Removes the tag if and only if there is no more image in this tag and in
-- all of its children. Removes the parent tag if this last become orphan too.
removeTagIfOrphan :: Tag -> IndexSTM ()
removeTagIfOrphan     (Tag RootTag              _ _) = return ()
removeTagIfOrphan tag@(Tag (SubTag name parent) _ _) = do
    orphan <- isEmpty tag
    when orphan $ do
        lift $ modifyTVar' (tSubTags parent) (M.delete name)
        removeTagIfOrphan parent
  where
    isEmpty Tag {..} = do
        imgs' <- lift $ readTVar tImages
        if S.null imgs' then     lift (readTVar tSubTags)
                             >>= (allM isEmpty . M.elems)
                        else return False

    allM _ []     = return True
    allM p (x:xs) = do ret <- p x
                       if ret then allM p xs
                              else return False

-- | Returns the set of images of the given tag and of its children.
getTagImages :: Tag -> IndexSTM (Set IndexedImage)
getTagImages tag =
    S.unions <$> lift (dfs [] tag)
  where
    dfs acc Tag {..} = do
        subs <- readTVar tSubTags
        imgs <- readTVar tImages
        foldM dfs (imgs : acc) (M.elems subs)

-- Image codes -----------------------------------------------------------------

-- | Number of characters in an 'ImageCode'.
imageCodeLength :: Int
imageCodeLength = 16

-- | Generates a new unique image code using the given random generator and
-- secret key. Returns the new state of the generator.
newImageCode :: RandomGen g
             => ByteString -> UserIndex -> g -> IndexSTM (ImageCode, g)
newImageCode key ui@(UserIndex {..}) gen = do
    imgs <- lift $ readTVar uiImages
    if code `M.member` imgs
        then newImageCode key ui gen'' -- Already used code, retries.
        else return (code, gen'')
   where
    -- Uses two 64 bits random number to generate a 128 bits random value.
    rand1, rand2 :: Int64
    (rand1, gen')  = random gen
    (rand2, gen'') = random gen'
    rand           = (C.pack $ show rand1) <> (C.pack $ show rand2)

    key' = key <> (C.pack $ T.unpack uiName)

    code = let hmac = integerDigest $ hmacSha1 key' rand
           in ImageCode $ T.pack $ take imageCodeLength $ toBase62 $ hmac

    -- | Encodes an integer in base 62 (using letters and numbers).
    toBase62 i = map ((digitToChar V.!) . fromInteger) $ digits 62 i

    digitToChar = V.fromList $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- Images ----------------------------------------------------------------------

-- | Creates an 'IndexedImage' object in the index and generates a new
-- 'ImageCode' for it.
newImage :: RandomGen g
         => ByteString -> UserIndex -> g -> Maybe Text -> [Tag]
         -> IndexedHistogram
         -> IndexSTM (IndexedImage, g)
newImage key ui gen name tags hist = do
    (code, gen') <- newImageCode key ui gen
    (,) <$> addImage ui code name tags hist <*> pure gen'

-- | Creates an 'IndexedImage' object in the index from an already generated
-- 'ImageCode'.
addImage :: UserIndex -> ImageCode -> Maybe Text -> [Tag] -> IndexedHistogram
        -> IndexSTM IndexedImage
addImage ui code name tags hist = do
    let !img = IndexedImage code name (S.fromList tags) hist
    bindImage ui img
    return img

lookupImage :: UserIndex -> ImageCode -> IndexSTM (Maybe IndexedImage)
lookupImage UserIndex {..} code = M.lookup code <$> lift (readTVar uiImages)

-- | Unbinds the given image from the given user and from all its tags so it's
-- no more referenced in the index. Removes tags which don't have any image
-- pointing them. Similar to 'unBindImage' but removes orphan tags.
removeImage :: UserIndex -> IndexedImage -> IndexSTM ()
removeImage ui img@(IndexedImage {..}) = do
    unBindImage ui img
    F.mapM_ removeTagIfOrphan iiTags

-- | Links the image to the given user and all its tags.
bindImage :: UserIndex -> IndexedImage -> IndexSTM ()
bindImage UserIndex {..} img@(IndexedImage {..}) = do
    lift $ modifyTVar' uiImages (M.insert iiCode img)
    if S.null iiTags then bindImageTag img uiRootTag
                     else F.mapM_ (bindImageTag img) iiTags

-- | Unbinds the given image from the given user and from all its tags so it's
-- no more referenced in the index.
unBindImage :: UserIndex -> IndexedImage -> IndexSTM ()
unBindImage UserIndex {..} img@(IndexedImage {..}) = do
    lift $ modifyTVar' uiImages (M.delete iiCode)
    if S.null iiTags then unBindImageTag img uiRootTag
                     else F.mapM_ (unBindImageTag img) iiTags

-- | Creates a link between the tag and the image. The 'IndexedImage' object is
-- not modified.
bindImageTag :: IndexedImage -> Tag -> IndexSTM ()
bindImageTag img Tag {..} = lift $ modifyTVar' tImages (S.insert img)

-- | Removes the link between the tag and the image. The 'IndexedImage' object
-- is not modified.
unBindImageTag :: IndexedImage -> Tag -> IndexSTM ()
unBindImageTag img Tag {..} = lift $ modifyTVar' tImages (S.delete img)

-- | Returns the set of images of the user.
getImages :: UserIndex -> IndexSTM (Set IndexedImage)
getImages = getTagImages . uiRootTag

-- Last called queue -----------------------------------------------------------

-- | Updates the last call time for the user index and updates the least
-- recently used queue.
-- O(n).
touchUserIndex :: ImageIndex -> UserIndex -> IndexSTM ()
touchUserIndex ii ui@(UserIndex {..}) = do
    detatchNode ii ui

    time <- ask
    _ <- attachNode ii (updateNode time)
    return ()
  where
    updateNode time mPrev mNext = do
        lift $ writeTVar uiLRCTime time
        lift $ writeTVar uiLRCPrev mPrev
        lift $ writeTVar uiLRCNext mNext
        return ui

-- | Calls the given action with the previous and next nodes of the
-- corresponding LRU queue position (determined by the current time) in which
-- the returned node will be inserted. Returns the inserted node. O(n).
attachNode :: ImageIndex
           -> (Maybe UserIndex -> Maybe UserIndex -> IndexSTM UserIndex)
           -> IndexSTM UserIndex
attachNode ImageIndex {..} idxFct =
    go Nothing iiLRCFirst
  where
    go mPrev nextRef = do
        mNext <- lift $ readTVar nextRef
        case mNext of
            Just next -> do
                currentTime <- ask
                time        <- lift $ readTVar (uiLRCTime next)
                if time < currentTime
                    then go mNext (uiLRCNext next)
                    else do
                        -- Inner/First node.
                        userIdx <- idxFct mPrev mNext
                        lift $ writeTVar nextRef          (Just userIdx)
                        lift $ writeTVar (uiLRCPrev next) (Just userIdx)
                        return userIdx
            Nothing -> do
                -- Last node.
                userIdx <- idxFct mPrev mNext
                lift $ writeTVar nextRef   (Just userIdx)
                lift $ writeTVar iiLRCLast (Just userIdx)
                return userIdx

-- Removes the specified node from the LRU queue.
-- O(1).
detatchNode :: ImageIndex -> UserIndex -> IndexSTM ()
detatchNode ImageIndex {..} userIdx = do
    prev <- lift $ readTVar (uiLRCPrev userIdx)
    next <- lift $ readTVar (uiLRCNext userIdx)

    case prev of
        Just userIdx' -> lift $ writeTVar (uiLRCNext userIdx') next
        Nothing       -> lift $ writeTVar iiLRCFirst           next

    case next of
        Just userIdx' -> lift $ writeTVar (uiLRCPrev userIdx') prev
        Nothing       -> lift $ writeTVar iiLRCLast            prev
