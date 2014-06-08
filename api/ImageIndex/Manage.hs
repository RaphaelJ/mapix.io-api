-- | Provides primitives to manage the in-memory transactionnal index.
module ImageIndex.STM ()
    where

import Control.Applicative
import Control.Concurrent.STM (modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Monad
import Control.Monad.STM
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

  :: IO ImageIndex
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

    newUserIndex prev next = do
        rootTag <- Tag RootTag <$> newTVar M.empty <*> newTVar S.empty
        UserIndex username <$> newTVar M.empty  <*> pure rootTag
                           <*> pure currentTime <*> newTVar prev
                           <*> newTVar next

removeUserIndex :: ImageIndex -> UserIndex -> STM ()
removeUserIndex ii@(ImageIndex {..}) ui@(UserIndex {..}) = do
    modifyTVar' iiUsers (M.delete uiName)
    detatchNode ii ui

-- Tags ------------------------------------------------------------------------

-- | Returns the last tag of the requested hierarchy of tags.
-- Creates tags which don\'t exist on the path.
getTag :: UserIndex -> TagPath -> STM Tag
getTag UserIndex {..} tagPath = do
    readTVar uiRootTag >>= dfs tagPath
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
lookupTag UserIndex {..} tagPath = do
    readTVar uiRootTag >>= dfs tagPath
  where
    dfs []       tag               = return $ Just tag
    dfs (t : ts) parent@(Tag {..}) =
        subTagsVal <- readTVar tSubTags
        case M.lookup t subTagsVal of
            Just tag -> dfs ts tag
            Nothing  -> return Nothing

-- | Removes the tag if and only if there is no more image in this tag and in
-- all of its children. Removes the parent tag if this one become orphan.
removeTagIfOrphan :: Tag -> STM ()
removeTagIfOrphan     (Tag RootTag              _ _) = return ()
removeTagIfOrphan tag@(Tag (SubTag name parent) _ _) = do
    orphan <- isEmpty tag
    when orphan $ do
        modifyTVar' (M.delete name) (tSubTags parent)
        removeTagIfOrphan parent
  where
    isEmpty tag'@(Tag {..}) = do
        imgs' <- readTVar tImages
        if S.null imgs' then readTVar tSubTags >>= allM isEmpty
                        else return False

    allM _ []     = True
    allM p (x:xs) = do ret <- p x
                       if ret then allM xs
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

-- Images ----------------------------------------------------------------------

-- | Binds the given image and all its tags to the given user.
addImage :: UserIndex -> Image -> STM ()
addImage ui@(UserIndex {..}) img@(Image {..}) = do
    modifyTVar' uiImages (M.insert iHmac img)
    mapM (bindImageTag img) iTags

lookupImage :: UserIndex -> Hmac -> STM (Maybe Image)
lookupImage UserIndex {..} hmac = M.lookup hmac <$> readTVar uiImages

-- | Unbinds the given image from the given users and from all its tags.
removeImage :: UserIndex -> Image -> STM ()
removeImage ui@(UserIndex {..}) img@(Image {..}) = do
    modifyTVar' uiImages (M.delete iHmac)
    mapM (unBindImageTag img) iTags

-- | Creates a link between the tag and the image.
bindImageTag :: Image -> Tag -> STM ()
bindImageTag img tag@(Tag {..}) = modifyTVar' tImages (S.insert img)

-- | Removes the link between the tag and the image. Removes the tag if no more
-- image are pointing to it.
unBindImageTag :: Tag -> Image -> STM ()
unBindImageTag img tag@(Tag {..}) = do
    modifyTVar' tImages (S.delete img)
    removeTagIfOrphan tag

-- | Returns the set of images of the user.
getImages :: UserIndex -> STM (Set Image)
getImages = getTagImages . uiRootTag

-- Last called queue -----------------------------------------------------------

-- | Updates the last call time for the user index and updates the least
-- recently used queue.
-- O(n).
touchUserIndex :: ImageIndex -> UserIndex -> UTCTime -> STM ()
touchUserIndex ImageIndex {..} ui@(UserIndex {..}) currentTime = do
    detatchNode lc ui
    attachNode  lc currentTime updateNode
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
attachNode :: ImageIndex -> UTCTime ->
           -> (Maybe UserIndex -> Maybe UserIndex -> STM UserIndex)
           -> STM UserIndex
attachNode ImageIndex {..} currentTime idxFct =
    go Nothing iiLRCFirst
  where
    go mPrev nextRef = do
        mNext <- readTVar nextRef
        case mNext of
            Just next | uiLRCTime next < currentTime -> go next (uiLRCNext next)
                      | otherwise                    -> do
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
