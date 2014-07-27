-- | Provides primitives to manage the in-memory transactional index.
module ImageIndex.Manage (
    -- * Index creation
      newIndex
    -- * User index management
    , getUserIndex, removeUserIndex, userIndexSize, touchUserIndex
    -- * Tag management
    , getTag, lookupTag, removeTag, removeTagIfOrphan, getTagImages
    -- * Images management
    , imageCodeLength, newImageCode
    , addImage, lookupImage, removeImage, bindImage, unBindImage, bindImageTag
    , unBindImageTag, getImages
    )
    where

import Prelude

import Control.Applicative
import Control.Concurrent.STM (
      modifyTVar', newTVar, newTVarIO, readTVar, writeTVar
    )
import Control.Monad
import Control.Monad.STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.SHA (hmacSha1, integerDigest)
import Data.Digits (digits)
import Data.Int
import qualified Data.Map as M
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import qualified Data.Vector.Storable as V
import System.Random (RandomGen, random)

import ImageIndex.Type

newIndex :: IO ImageIndex
newIndex = ImageIndex <$> newTVarIO M.empty <*> newTVarIO Nothing
                      <*> newTVarIO Nothing

-- Users -----------------------------------------------------------------------

-- | Searches for an existing user index entry by the user name and returns it.
-- If a such entry doesn\'t exist, creates a new one.
getUserIndex :: ImageIndex -> UserName -> UTCTime -> STM UserIndex
getUserIndex ii@(ImageIndex {..}) username currentTime = do
    iiUsersVal <- readTVar iiUsers
    case M.lookup username iiUsersVal of
        Just userIdx -> return userIdx
        Nothing      -> do
            userIdx <- attachNode ii currentTime newUserIndex
            writeTVar iiUsers (M.insert username userIdx iiUsersVal)

            return userIdx
  where
    newUserIndex prev next = do
        rootTag <- Tag RootTag <$> newTVar M.empty <*> newTVar S.empty
        UserIndex username <$> newTVar M.empty     <*> pure rootTag
                           <*> newTVar currentTime <*> newTVar prev
                           <*> newTVar next

removeUserIndex :: ImageIndex -> UserIndex -> STM ()
removeUserIndex ii@(ImageIndex {..}) ui@(UserIndex {..}) = do
    modifyTVar' iiUsers (M.delete uiName)
    detatchNode ii ui

userIndexSize :: UserIndex -> STM Int
userIndexSize UserIndex {..} = M.size <$> readTVar uiImages

-- Tags ------------------------------------------------------------------------

-- | Returns the last tag of the requested hierarchy of tags.
-- Creates tags which don\'t exist on the path.
getTag :: UserIndex -> TagPath -> STM Tag
getTag UserIndex {..} (TagPath path) = do
    dfs path uiRootTag
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
lookupTag :: UserIndex -> TagPath -> STM (Maybe Tag)
lookupTag UserIndex {..} (TagPath path) = do
    dfs path uiRootTag
  where
    dfs []       tag      = return $ Just tag
    dfs (t : ts) Tag {..} = do
        subTagsVal <- readTVar tSubTags
        case M.lookup t subTagsVal of
            Just tag -> dfs ts tag
            Nothing  -> return Nothing

-- | Removes the tag and its subtags. Dereferences their images. Removes the
-- parent tag if it has become orphan. Does nothing for the root tag.
removeTag :: UserIndex -> Tag -> STM ()
removeTag _  (Tag RootTag              _ _)     = return ()
removeTag ui tag@(Tag (SubTag name parent) _ _) = do
    dfs tag
    modifyTVar' (tSubTags parent) (M.delete name)
    removeTagIfOrphan parent
  where
    dfs tag'@(Tag {..}) = do
        readTVar tSubTags >>= (mapM_ dfs . M.elems)

        imgs <- readTVar tImages
        forM_ (S.toList imgs) $ \img -> do
            unBindImage ui img
            bindImage ui img { iTags = S.delete tag' (iTags img) }

-- | Removes the tag if and only if there is no more image in this tag and in
-- all of its children. Removes the parent tag if this last become orphan too.
removeTagIfOrphan :: Tag -> STM ()
removeTagIfOrphan     (Tag RootTag              _ _) = return ()
removeTagIfOrphan tag@(Tag (SubTag name parent) _ _) = do
    orphan <- isEmpty tag
    when orphan $ do
        modifyTVar' (tSubTags parent) (M.delete name)
        removeTagIfOrphan parent
  where
    isEmpty Tag {..} = do
        imgs' <- readTVar tImages
        if S.null imgs' then readTVar tSubTags >>= (allM isEmpty . M.elems)
                        else return False

    allM _ []     = return True
    allM p (x:xs) = do ret <- p x
                       if ret then allM p xs
                              else return False

-- | Returns the set of images of the given tag and of its children.
getTagImages :: Tag -> STM (Set Image)
getTagImages tag =
    S.unions <$> dfs [] tag
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
             => ByteString -> UserIndex -> g -> STM (ImageCode, g)
newImageCode key ui@(UserIndex {..}) gen = do
    imgs <- readTVar uiImages
    if code `M.member` imgs
        then return (code, gen'')
        else newImageCode key ui gen'' -- Already used code, retries.
   where
    -- Uses two 64 bits random number to generate a 128 bits random value.
    rand1, rand2 :: Int64
    (rand1, gen')  = random gen
    (rand2, gen'') = random gen'

    key' = key <> (C.pack $ T.unpack uiName)

    code = let rand' = (C.pack $ show rand1) <> (C.pack $ show rand2)
               hmac  = integerDigest $ hmacSha1 key' rand'
           in ImageCode $ T.pack $ take imageCodeLength $ toBase62 $ hmac

    -- | Encodes an integer in base 62 (using letters and numbers).
    toBase62 i = map ((digitToChar V.!) . fromInteger) $ digits 62 i

    digitToChar = V.fromList $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- Images ----------------------------------------------------------------------

addImage :: RandomGen g
        => ByteString -> UserIndex -> g -> Maybe Text -> [Tag] -> Histogram
        -> STM (Image, g)
addImage key ui@(UserIndex {..}) gen name tags hist = do
    (code, gen') <- newImageCode key ui gen
    let !img = Image code name (S.fromList tags) hist
    bindImage ui img
    return (img, gen')

lookupImage :: UserIndex -> ImageCode -> STM (Maybe Image)
lookupImage UserIndex {..} code = M.lookup code <$> readTVar uiImages

-- | Unbinds the given image from the given user and from all its tags so it's
-- no more referenced in the index. Removes tags which don't have any image
-- pointing them. Similar to 'unBindImage' but removes orphan tags.
removeImage :: UserIndex -> Image -> STM ()
removeImage ui img@(Image {..}) = do
    unBindImage ui img
    mapM_ removeTagIfOrphan (S.toList iTags)

-- | Links the image to the given user and all its tags.
bindImage :: UserIndex -> Image -> STM ()
bindImage UserIndex {..} img@(Image {..}) = do
    modifyTVar' uiImages (M.insert iCode img)
    if S.null iTags then bindImageTag img uiRootTag
                    else mapM_ (bindImageTag img) (S.toList iTags)

-- | Unbinds the given image from the given user and from all its tags so it's
-- no more referenced in the index.
unBindImage :: UserIndex -> Image -> STM ()
unBindImage UserIndex {..} img@(Image {..}) = do
    modifyTVar' uiImages (M.delete iCode)
    if S.null iTags then unBindImageTag img uiRootTag
                    else mapM_ (unBindImageTag img) (S.toList iTags)

-- | Creates a link between the tag and the image. The 'Image' object is not
-- modified.
bindImageTag :: Image -> Tag -> STM ()
bindImageTag img Tag {..} = modifyTVar' tImages (S.insert img)

-- | Removes the link between the tag and the image. The 'Image' object is not
-- modified.
unBindImageTag :: Image -> Tag -> STM ()
unBindImageTag img Tag {..} = modifyTVar' tImages (S.delete img)

-- | Returns the set of images of the user.
getImages :: UserIndex -> STM (Set Image)
getImages = getTagImages . uiRootTag

-- Last called queue -----------------------------------------------------------

-- | Updates the last call time for the user index and updates the least
-- recently used queue.
-- O(n).
touchUserIndex :: ImageIndex -> UserIndex -> UTCTime -> STM ()
touchUserIndex ii ui@(UserIndex {..}) currentTime = do
    detatchNode ii ui
    _ <- attachNode  ii currentTime updateNode
    return ()
  where
    updateNode mPrev mNext = do
        writeTVar uiLRCTime currentTime
        writeTVar uiLRCPrev mPrev
        writeTVar uiLRCNext mNext
        return ui

-- | Calls the given action with the previous and next nodes of the
-- corresponding LRU queue position (determined by @currentTime@) in which the
-- returned node will be inserted. Returns the inserted node.
-- O(n).
attachNode :: ImageIndex -> UTCTime
           -> (Maybe UserIndex -> Maybe UserIndex -> STM UserIndex)
           -> STM UserIndex
attachNode ImageIndex {..} currentTime idxFct =
    go Nothing iiLRCFirst
  where
    go mPrev nextRef = do
        mNext <- readTVar nextRef
        case mNext of
            Just next -> do
                time <- readTVar (uiLRCTime next)
                if time < currentTime
                    then go mNext (uiLRCNext next)
                    else do
                        -- Inner/First node.
                        userIdx <- idxFct mPrev mNext
                        writeTVar nextRef          (Just userIdx)
                        writeTVar (uiLRCPrev next) (Just userIdx)
                        return userIdx
            Nothing -> do
                -- Last node.
                userIdx <- idxFct mPrev mNext
                writeTVar nextRef   (Just userIdx)
                writeTVar iiLRCLast (Just userIdx)
                return userIdx

-- Removes the specified node from the LRU queue.
-- O(1).
detatchNode :: ImageIndex -> UserIndex -> STM ()
detatchNode ImageIndex {..} userIdx = do
    prev <- readTVar (uiLRCPrev userIdx)
    next <- readTVar (uiLRCNext userIdx)

    case prev of
        Just userIdx' -> writeTVar (uiLRCNext userIdx') next
        Nothing       -> writeTVar iiLRCFirst           next

    case next of
        Just userIdx' -> writeTVar (uiLRCPrev userIdx') prev
        Nothing       -> writeTVar iiLRCLast            prev
