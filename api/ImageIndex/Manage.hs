module ImageIndex.Manage ()
    where

import Control.Applicative
import Control.Concurrent.STM (modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Monad
import Control.Monad.STM
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

newIndex :: IO ImageIndex
newIndex = ImageIndex <$> newTVarIO M.empty <*> newTVarIO Nothing
                      <*> newTVarIO Nothing

-- Users -----------------------------------------------------------------------

-- | Searches for an existing user index entry by the user name and returns it.
-- If a such entry doesn\'t exist, creates a new one.
getUserIndex :: ImageIndex -> UserName -> UTCTime -> STM UserIndex
getUserIndex ii@(ImageIndex {..}) username currentTime = do
    iiUserVal <- readTVar iiUsers
    case M.lookup username iiUserVal of
        Just userIdx -> return userIdx
        Nothing      -> do
            userIdx <- attachNode ii currentTime newUserIndex
            writeTVar iiUsers (M.insert username userIdx iiUserVal)

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
getTag :: UserIndex -> [TagName] -> STM Tag
getTag UserIndex {..} tagPath = do
    readTVar uiRootTag >>= dfs tagPath
  where
    dfs []       tag               = return tag
    dfs (t : ts) parent@(Tag {..}) = do
        subTags <- readTVar tSubTags
        tag <- case M.lookup t subTags of
            Just tag -> return tag
            Nothing  -> do -- Sub tag doesn't exist.
                tag <- Tag (SubTag tagName parent) <$> newTVar M.empty
                                                   <*> newTVar S.empty
                writeTVar tSubTags (M.insert TagName)
                return tag
        dfs ts tag

-- | Returns the last tag of the requested hierarchy of tags if the whole
-- hierarchy exists.
lookupTag :: UserIndex -> [TagName] -> STM (Maybe Tag)
lookupTag UserIndex {..} tagPath = do
    readTVar uiRootTag >>= dfs tagPath
  where
    dfs []       tag               = return $! Just tag
    dfs (t : ts) parent@(Tag {..}) =
        subTags <- readTVar tSubTags
        case M.lookup t subTags of
            Just tag -> dfs ts tag
            Nothing  -> return Nothing

-- | Removes a tag and its sub-tags. Unregisters every image links to those
-- tags. Does nothing for the root tag.
removeTag :: UserIndex -> Tag -> STM ()
removeTag _                       (Tag RootTag              _       _   ) =
    return ()
removeTag ui@(UserIndex {..}) tag@(Tag (SubTag name parent) subTags imgs) = do
    unBindParent parent
    (M.elems  <$> readTVar subTags) >>= unBindSubs
    (S.toList <$> readTVar imgs)    >>= unBindImages
  where
    unBindParent Tag {..} = do
        subTags' <- M.delete name <$> readTVar tSubTags
        imgs'    <- readTVar tImages
        writeTVar tSubTags subTags'

        when (M.null subTags' && S.null imgs') $
            removeTag parent

    unBindSubs = mapM_ (removeTag ui)

    unBindImages = mapM_ (unBindImageTag' ui tag)

-- | Calls 'removeTag' if and only there is no more images in this tag and in
-- all of its children.
removeTagIfOrphan :: UserIndex -> Tag -> STM ()
removeTagIfOrphan _      (Tag RootTag _ _  _ _) = return ()
removeTagIfOrphan ui tag@(Tag (SubTag _ _) _ _) = do
    orphan <- isEmpty tag
    if orphan then removeTag ui tag
                else return ()
  where
    isEmpty tag'@(Tag {..}) = do
        imgs'       <- readTVar tImages
        subsAreEmpty <- readTVar tSubTags >>= allM isEmpty
        return $! S.null imgs' && subsAreEmpty

    allM _ []     = True
    allM p (x:xs) = do ret <- p x
                       if ret then allM xs
                              else return False

-- | Creates a link between the tag and the image.
bindImageTag :: UserIndex -> Tag -> Image -> STM ()
bindImageTag tag@(Tag {..}) img = modifyTVar' tImages (S.insert img)

-- | Removes the link between the tag and the image. Doesn't remove the tag if
-- no more image is pointing to it.
unBindImageTag :: UserIndex -> Tag -> Image -> STM ()
unBindImageTag ui@(UserIndex {..}) tag@(Tag {..}) img =
    modifyTVar' tImages (S.delete img)

-- | Returns the sets of images of the given tag and of its children.
getTagImages :: Tag -> STM [Set Image]
getTagImages =
    dfs []
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
    mapM (\tag -> unBindImageTag ui tag img) iTags

lookupImage :: UserIndex -> Hmac -> STM (Maybe Image)
lookupImage UserIndex {..} hmac = M.lookup hmac <$> readTVar uiImages

-- | Unbinds the given image and all its tags from the given user.
removeImage :: UserIndex -> Image -> STM ()
removeImage ui@(UserIndex {..}) img@(Image {..}) = do
    modifyTVar' uiImages (M.delete iHmac)
    mapM (\tag -> unBindImageTag ui tag img) iTags

-- Last called queue -----------------------------------------------------------

-- | Updates the last call time for the user index and updates the least
-- recently used queue.
-- O(n).
touchUserIndex :: ImageIndex -> UserIndex -> UTCTime -> STM ()
touchUserIndex ImageIndex {..} username currentTime = do
    mNode <- M.lookup username <$> readTVar (lcMap iiLastCalled)
    case mNode of
        Just node -> do
            detatchNode lc node
            attachNode  lc currentTime (updateNode node)
        Nothing -> return ()
  where
    updateNode node@(LastCalledNode {..}) mPrev mNext = do
        writeTVar lcnTime currentTime
        writeTVar lcnPrev mPrev
        writeTVar lcnNext mNext
        return node

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
                writeTVar (uiLRCPrev next) (Just node)
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
