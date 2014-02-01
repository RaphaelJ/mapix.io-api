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
newIndex = do
    lastCalled <- LastCalledQueue <$> newTVarIO Nothing <*> newTVarIO Nothing
                                  <*> newTVarIO M.empty
    ImageIndex <$> newTVarIO M.empty <*> pure lastCalled

-- Users -----------------------------------------------------------------------

-- | Searches for an existing user index entry by the user name and returns it.
-- If a such entry doesn\'t exist, creates a new one.
getUserIndex :: ImageIndex -> UserName -> UTCTime -> STM UserIndex
getUserIndex ImageIndex {..} username currentTime = do
    iiUserVal <- readTVar iiUsers
    case M.lookup username iiUserVal of
        Just userIdx -> return userIdx
        Nothing      -> do
            -- Creates the user index.
            rootTag <- Tag RootTag <$> newTVar M.empty <*> newTVar S.empty
            userIdx <- UserIndex username <$> newTVar M.empty <*> pure rootTag
            writeTVar iiUsers (M.insert username userIdx iiUserVal)

            -- Inserts the new user index in the last call queue.
            node <- attachNode iiLastCalled currentTime (newNode userIdx)
            modifyTVar (lcMap iiLastCalled) (M.insert username node)

            return userIdx

    newNode userIdx prev next =
        LastCalledNode userIdx <$> newTVar currentTime <*> newTVar prev
                               <*> newTVar next

removeUserIndex :: ImageIndex -> UserIndex -> STM ()
removeUserIndex ImageIndex {..} UserIndex {..} = do
    modifyTVar' iiUsers (M.delete uiName)
    detatchNode (uiName  

-- Tags ------------------------------------------------------------------------

-- | Returns the last tag of the requested hierarchy of tags.
-- Creates tags which don\'t exist on the path.
getTag :: UserIndex -> [TagName] -> STM Tag
getTag UserIndex {..} tagPath = do
    readTVar uiRootTag >>= dfs tagPath
  where
    dfs []       tag               = return tag
    dfs (t : ts) parent@(Tag {..}) =
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
    unBindParent Nothing       = return ()
    unBindParent (Just Tag {..}) = do
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
-- recently used queue. Does nothing if the user index doesn\'t exist.
-- O(n).
touchUserIndex :: LastCalledQueue -> UserName -> UTCTime -> STM ()
touchUserIndex lc@(LastCalledQueue {..}) username currentTime = do
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
attachNode :: LastCalledQueue -> UTCTime ->
           -> (Maybe LastCalledNode -> Maybe LastCalledNode
               -> STM LastCalledNode)
           -> STM LastCalledNode
attachNode LastCalledQueue {..} currentTime nodeFct =
    go Nothing lcFirst
  where
    go mPrev nextRef = do
        mNext <- readTVar nextRef
        case mNext of
            Just next | lcnTime next < currentTime -> go next (lcnNext next)
                      | otherwise                  -> do
                -- Inner/First node.
                node <- nodeFct mPrev mNext
                writeTVar nextRef        (Just node)
                writeTVar (lcnPrev next) (Just node)
                return node
            Nothing -> do
                -- Last node.
                node <- nodeFct mPrev mNext
                writeTVar nextRef               (Just node)
                writeTVar (lcLast iiLastCalled) (Just node)
                return node

-- Removes the specified node from the LRU queue.
-- O(1).
detatchNode :: ImageIndex -> LastCalledNode -> STM ()
detatchNode LastCalledQueue {..} node = do
    prev <- readTVar (lcnPrev node)
    next <- readTVar (lcnNext node)

    case prev of
        Just node' -> writeTVar (lcnNext node') next
        Nothing    -> writeTVar lcFirst         next

    case next of
        Just node' -> writeTVar (lcnPrev node') prev
        Nothing    -> writeTVar lcLast          prev
