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
    lastCalled <- LastCalled <$> newTVarIO Nothing <*> newTVarIO Nothing
                             <*> newTVarIO M.empty
    ImageIndex <$> newTVarIO M.empty <*> newTVarIO M.empty <*> pure lastCalled

-- Users -----------------------------------------------------------------------

-- | Searches for an existing user index entry by the user name and returns it.
-- If a such entry doesn\'t exists, creates a new one.
getUserIndex :: ImageIndex -> UserName -> UTCTime -> STM UserIndex
getUserIndex ImageIndex {..} username currentTime = do
    iiUserVal <- readTVar iiUsers
    case M.lookup username iiUserVal of
        Just userIdx -> return userIdx
        Nothing      -> do
            rootTag <- Tag RootTag <$> newTVar M.empty <*> newTVar S.empty
            userIdx <- UserIndex username rootTag <*> newTVar M.empty
            writeTVar (M.insert username userIdx iiUserVal)

            lastCallNode <- LastCalledNode <$> newTVar currentTime
                                           <*> newTVar Nothing
                                           <*>

            return userIdx

-- | Updates the last call time for the user index and updates the least
-- recently used queue.
touchUserIndex :: ImageIndex -> UserIndex -> UTCTime -> STM ()
touchUserIndex ImageIndex {..} UserIndex {..} currentTime = do
    Just node <- M.lookup uiName <$> readTVar (lcMap iiLastCalled)
    detatchNode node
    writeTVar (lcnTime node) currentTime
    attachNode node
  where
    -- Removes the LastCalledNode from the LRU queue.
    detatchNode node = do
        prev <- readTVar (lcnPrev node)
        next <- readTVar (lcnNext node)

        case prev of
            Just node' -> writeTVar (lcnNext node') next
            Nothing    -> writeTVar (lcFirst iiLastCalled) next

        case next of
            Just node' -> writeTVar (lcnPrev node') prev
            Nothing    -> writeTVar (lcLast iiLastCalled) prev

    -- Adds the LastCalledNode to the LRU queue and keeps the queue sorted.
    attachNode node prevVar = do
        prev <- readTVar (lcnPrev node)
        case prev of
            Just prev ->
            Nothing   -> 

-- Tags ------------------------------------------------------------------------

-- | Returns the last tag of the requested hierarchy of tags.
-- Creates tags which don\'t exist on the path.
getTag :: UserIndex -> [TagName] -> STM Tag
getTag UserIndex {..} tagPath = do
    dfs tagPath uiRootTag
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
removeTag :: UserIndex -> Tag SubTag -> STM ()
removeTag UserIndex {..} (Tag RootTag              _       _     ) = return ()
removeTag UserIndex {..} (Tag (SubTag name parent) subTags images) = do
    unBindParent parent
    (M.elems  <$> readTVar subTags) >>= unBindSubs
    (S.toList <$> readTVar images)  >>= unBindImages
  where
    unBindParent Nothing       = return ()
    unBindParent (Just parent) = do
        subTags' <- M.delete name <$> readTVar (tSubTags parent)
        if M.null subTags' then removeTag parent
                           else writeTVar (tSubTags parent) subTags'

    unBindSubs subs = mapM_ removeTag subs

    unBindImages images =
        forM_ images $ \image@(Image {..}) -> do
            iTags' <- S.delete tag <$> readTVar iTags
            when (S.null iTags') $
                -- When the image is no more referenced anywhere, we need to
                -- add it to the user's root tag.
                modifyTVar' (tImages uiRootTag) (S.insert image)
            writeTVar iTags iTags'

-- Histograms ------------------------------------------------------------------

-- | Searches for an existing histogram by its hash.
-- If a such entry doesn\'t exists, creates a new one.
getHistogram :: HistogramHash -> H.Histogram DIM5 Float
             -> H.Histogram DIM3 Float -> STM Histogram
getHistogram hash hist5D hist3D = do
    iiHistVal <- readTVar iiHists
    case M.lookup hash iiHistVal of
        Just hist -> return hist
        Nothing   -> do
            hist <- Histogram hash hist5D hist3D <$> newTVar 0
            writeTVar (M.insert hash hist iiHistVal)
            return hist

-- Images ----------------------------------------------------------------------

-- | Allocates a new image and binds it to the given users and the given tags.
-- Increments by one the 'Histogram' counter.
newImage :: UserIndex -> Hmac -> Maybe Text -> [Tag] -> Histogram
         -> STM Image
newImage UserIndex {..} hmac mName tags hist@(Histogram {..}) = do
    image <- Image hmac <$> newTVar mName <*> newTVar tags <*> pure hist

    modifyTVar' uiImages (M.insert hmac image)

    forM_ tags $ \Tag {..} -> do
        modifyTVar' utImages (S.insert image)

    modifyTVar' hCount (+ 1)

    return image

lookupImage :: UserIndex -> Hmac -> STM (Maybe Image)
lookupImage UserIndex {..} hmac = M.lookup hmac <$> readTVar uiImages

removeImage :: ImageIndex -> UserIndex -> Image -> STM ()
removeImage ImageIndex {..} UserIndex {..} Image {..} = do
    modifyTVar' uiImages (M.delete iHmac)
    readTVar iTags >>= unBindTags
    readTVar iHist >>= unBindHist
  where
    unBindTags = 

    unBindHist Histogram {..} = do
        count <- readTVar hCount
        if count <= 1 then modifyTVar' iiHists (M.delete hHash)
                      else writeTVar hCount (count - 1)
