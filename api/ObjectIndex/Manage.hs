-- | Provides primitives to manage the in-memory transactional index.
module ObjectIndex.Manage (
    -- * Index creation
      newIndex
    -- * Index transactions
    , runTransaction, liftSTM
    -- * User index management
    , getUserIndex, removeUserIndex, userIndexSize, touchUserIndex
    -- * Tag management
    , getTag, lookupTag, removeTag, removeTagIfOrphan, getTagObjects
    -- * Objects management
    , objectCodeLength, newObjectCode
    , addObject, newObject, lookupObject, removeObject, bindObject
    , unBindObject, bindObjectTag, unBindObjectTag, getObjects
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
import Data.Digest.Pure.SHA (hmacSha1, integerDigest)
import Data.Digits (digits)
import Data.Int
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import System.Random (RandomGen, random)

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Foldable              as F
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Vector.Storable       as V

import ObjectIndex.Type

newIndex :: MonadIO m => m ObjectIndex
newIndex = liftIO $ ObjectIndex <$> newTVarIO M.empty <*> newTVarIO Nothing
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
getUserIndex :: ObjectIndex -> UserName -> IndexSTM UserIndex
getUserIndex oi@(ObjectIndex {..}) username = do
    time        <- ask
    oiUsersVal  <- lift $ readTVar oiUsers
    case M.lookup username oiUsersVal of
        Just userIdx -> return userIdx
        Nothing      -> do
            userIdx <- attachNode oi (newUserIndex time)
            lift $ writeTVar oiUsers (M.insert username userIdx oiUsersVal)

            return userIdx
  where
    newUserIndex time prev next = lift $ do
        rootTag <- Tag RootTag <$> newTVar M.empty <*> newTVar S.empty
        UserIndex username <$> newTVar M.empty <*> pure rootTag
                           <*> newTVar time    <*> newTVar prev
                           <*> newTVar next

removeUserIndex :: ObjectIndex -> UserIndex -> IndexSTM ()
removeUserIndex oi@(ObjectIndex {..}) ui@(UserIndex {..}) = do
    lift $ modifyTVar' oiUsers (M.delete uiName)
    detatchNode oi ui

userIndexSize :: UserIndex -> IndexSTM Int
userIndexSize UserIndex {..} = M.size <$> lift (readTVar uiObjects)

-- Tags ------------------------------------------------------------------------

-- | Returns the last tag (last child) of the requested hierarchy of tags.
--
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

-- | Returns the last tag (last child) of the requested hierarchy of tags if the
-- whole hierarchy exists.
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

-- | Removes the tag and its subtags. Dereferences their associated objects.
-- Removes the parent tag if it has become orphan and if it's not the root tag.
removeTag :: UserIndex -> Tag -> IndexSTM ()
removeTag _  (Tag RootTag              _ _)     = return ()
removeTag ui tag@(Tag (SubTag name parent) _ _) = do
    dfs tag
    lift $ modifyTVar' (tSubTags parent) (M.delete name)
    removeTagIfOrphan parent
  where
    dfs tag'@(Tag {..}) = do
        lift (readTVar tSubTags) >>= (F.mapM_ dfs)

        objs <- lift $ readTVar tObjects
        F.forM_ objs $ \obj -> do
            unBindObject ui obj
            bindObject ui obj { ioTags = S.delete tag' (ioTags obj) }

-- | Removes the tag if and only if there is no more objects linked to it or any
-- of its children. Removes the parent tag if this last become orphan too.
removeTagIfOrphan :: Tag -> IndexSTM ()
removeTagIfOrphan     (Tag RootTag              _ _) = return ()
removeTagIfOrphan tag@(Tag (SubTag name parent) _ _) = do
    orphan <- isEmpty tag
    when orphan $ do
        lift $ modifyTVar' (tSubTags parent) (M.delete name)
        removeTagIfOrphan parent
  where
    isEmpty Tag {..} = do
        objs' <- lift $ readTVar tObjects
        if S.null objs' then     lift (readTVar tSubTags)
                             >>= (allM isEmpty . M.elems)
                        else return False

    allM _ []     = return True
    allM p (x:xs) = do ret <- p x
                       if ret then allM p xs
                              else return False

-- | Returns the set of object of the given tag and of its children.
getTagObjects :: Tag -> IndexSTM (Set IndexedObject)
getTagObjects tag =
    S.unions <$> lift (dfs [] tag)
  where
    dfs acc Tag {..} = do
        subs <- readTVar tSubTags
        objs <- readTVar tObjects
        foldM dfs (objs : acc) (M.elems subs)

-- Object codes -----------------------------------------------------------------

-- | Number of characters in an 'ObjectCode'.
objectCodeLength :: Int
objectCodeLength = 16

-- | Generates a new unique object code using the given random generator and
-- secret key. Returns the new state of the generator.
newObjectCode :: RandomGen g
              => ByteString -> UserIndex -> g -> IndexSTM (ObjectCode, g)
newObjectCode key ui@(UserIndex {..}) gen = do
    objs <- lift $ readTVar uiObjects
    if code `M.member` objs
        then newObjectCode key ui gen'' -- Already used code, retries.
        else return (code, gen'')
   where
    -- Uses two 64 bits random number to generate a 128 bits random value.
    rand1, rand2 :: Int64
    (rand1, gen')  = random gen
    (rand2, gen'') = random gen'
    rand           = (C.pack $ show rand1) <> (C.pack $ show rand2)

    key' = key <> (C.pack $ T.unpack uiName)

    code = let hmac = integerDigest $ hmacSha1 key' rand
           in ObjectCode $ T.pack $ take objectCodeLength $ toBase62 $ hmac

    -- | Encodes an integer in base 62 (using letters and numbers).
    toBase62 i = map ((digitToChar V.!) . fromInteger) $ digits 62 i

    digitToChar = V.fromList $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- Objects ---------------------------------------------------------------------

-- | Creates an 'IndexedObject' object in the index and generates a new
-- 'ObjectCode' for it.
newObject :: RandomGen g
          => ByteString -> UserIndex -> g -> Maybe Text -> [Tag]
          -> IndexedHistogram
          -> IndexSTM (IndexedObject, g)
newObject key ui gen name tags hist = do
    (code, gen') <- newObjectCode key ui gen
    (,) <$> addObject ui code name tags hist <*> pure gen'

-- | Creates an 'IndexedObject' object in the user index from an already
-- generated 'ObjectCode'.
addObject :: UserIndex -> ObjectCode -> Maybe Text -> [Tag] -> IndexedHistogram
          -> IndexSTM IndexedObject
addObject ui code name tags hist = do
    let !obj = IndexedObject code name (S.fromList tags) hist
    bindObject ui obj
    return obj

lookupObject :: UserIndex -> ObjectCode -> IndexSTM (Maybe IndexedObject)
lookupObject UserIndex {..} code = M.lookup code <$> lift (readTVar uiObjects)

-- | Unbinds the given object from the given user and from all its tags so it's
-- no more referenced in the index. Removes tags which don't have any object
-- pointing them. Similar to 'unBindObject' but removes orphan tags.
removeObject :: UserIndex -> IndexedObject -> IndexSTM ()
removeObject ui obj@(IndexedObject {..}) = do
    unBindObject ui obj
    F.mapM_ removeTagIfOrphan ioTags

-- | Links the object to the given user and all its tags.
bindObject :: UserIndex -> IndexedObject -> IndexSTM ()
bindObject UserIndex {..} obj@(IndexedObject {..}) = do
    lift $ modifyTVar' uiObjects (M.insert ioCode obj)
    if S.null ioTags then bindObjectTag obj uiRootTag
                     else F.mapM_ (bindObjectTag obj) ioTags

-- | Unbinds the given object from the given user and from all its tags so it's
-- no more referenced in the index.
unBindObject :: UserIndex -> IndexedObject -> IndexSTM ()
unBindObject UserIndex {..} obj@(IndexedObject {..}) = do
    lift $ modifyTVar' uiObjects (M.delete ioCode)
    if S.null ioTags then unBindObjectTag obj uiRootTag
                     else F.mapM_ (unBindObjectTag obj) ioTags

-- | Creates a link between the tag and the object. The 'IndexedObject' object
-- is not modified.
bindObjectTag :: IndexedObject -> Tag -> IndexSTM ()
bindObjectTag obj Tag {..} = lift $ modifyTVar' tObjects (S.insert obj)

-- | Removes the link between the tag and the object. The 'IndexedObject' object
-- is not modified.
unBindObjectTag :: IndexedObject -> Tag -> IndexSTM ()
unBindObjectTag obj Tag {..} = lift $ modifyTVar' tObjects (S.delete obj)

-- | Returns the set of objects of the user.
getObjects :: UserIndex -> IndexSTM (Set IndexedObject)
getObjects = getTagObjects . uiRootTag

-- Last called queue -----------------------------------------------------------

-- | Updates the last call time for the user index and updates the least
-- recently used queue.
--
-- O(n).
touchUserIndex :: ObjectIndex -> UserIndex -> IndexSTM ()
touchUserIndex oi ui@(UserIndex {..}) = do
    detatchNode oi ui

    time <- ask
    _ <- attachNode oi (updateNode time)
    return ()
  where
    updateNode time mPrev mNext = do
        lift $ writeTVar uiLRUTime time
        lift $ writeTVar uiLRUPrev mPrev
        lift $ writeTVar uiLRUNext mNext
        return ui

-- | Calls the given action with the previous and next nodes of the
-- corresponding LRU queue position (determined by the current time) in which
-- the returned 'UserIndex' node will be inserted. Returns the inserted node.
--
-- O(n).
attachNode :: ObjectIndex
           -> (Maybe UserIndex -> Maybe UserIndex -> IndexSTM UserIndex)
           -> IndexSTM UserIndex
attachNode ObjectIndex {..} idxFct =
    go Nothing oiLRUFirst
  where
    go mPrev nextRef = do
        mNext <- lift $ readTVar nextRef
        case mNext of
            Just next -> do
                currentTime <- ask
                time        <- lift $ readTVar (uiLRUTime next)

                -- A concurrent transaction that have called getCurrentTime
                -- later could already have its node inserted into the LRU
                -- queue.
                -- We need to attach the node after such nodes to keep the queue
                -- ordered by a decreasing time.
                if currentTime < time
                    then go mNext (uiLRUNext next)
                    else do
                        -- Inner/First node.
                        userIdx <- idxFct mPrev mNext
                        lift $ writeTVar nextRef          (Just userIdx)
                        lift $ writeTVar (uiLRUPrev next) (Just userIdx)
                        return userIdx
            Nothing -> do
                -- Last node.
                userIdx <- idxFct mPrev mNext
                lift $ writeTVar nextRef   (Just userIdx)
                lift $ writeTVar oiLRULast (Just userIdx)
                return userIdx

-- Removes the specified 'UserIndex' node from the LRU queue.
--
-- O(1).
detatchNode :: ObjectIndex -> UserIndex -> IndexSTM ()
detatchNode ObjectIndex {..} userIdx = do
    prev <- lift $ readTVar (uiLRUPrev userIdx)
    next <- lift $ readTVar (uiLRUNext userIdx)

    case prev of
        Just userIdx' -> lift $ writeTVar (uiLRUNext userIdx') next
        Nothing       -> lift $ writeTVar oiLRUFirst           next

    case next of
        Just userIdx' -> lift $ writeTVar (uiLRUPrev userIdx') prev
        Nothing       -> lift $ writeTVar oiLRULast            prev
