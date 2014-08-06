module Handler.Tag (getTagsR, getTagR, deleteTagR) where

import Import

import Control.Concurrent.STM (readTVar)
import Control.Monad.STM (STM, atomically)
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types.Status (noContent204)

import Handler.Internal.Mashape (getMashapeHeaders, mhUser)
import Handler.Internal.Json ()
import Handler.Internal.Type (StaticTag (..))
import ImageIndex (
      Tag (..), TagPath, getUserIndex, lookupTag, removeTag, uiRootTag
    )

-- Handlers --------------------------------------------------------------------

-- | Returns the hierarchy of tags.
getTagsR :: Handler Value
getTagsR = do
    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    rootTag <- liftIO $ atomically $ do
        ui <- getUserIndex ii username currentTime
        getStaticTag $ uiRootTag ui

    returnJson rootTag

-- | Returns the tag and its sub-tags. Fails with a '404 Not Found' error if the
-- tag doesn't exist.
getTagR :: TagPath -> Handler Value
getTagR path = do
    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    mTag <- liftIO $ atomically $ do
        ui   <- getUserIndex ii username currentTime
        mTag <- lookupTag ui path
        case mTag of Just tag -> Just <$> getStaticTag tag
                     Nothing  -> return Nothing

    maybe notFound returnJson mTag

-- | Removes this tag and its subtags without removing the images. Returns a
-- '204 No content' on success. Fails with a '404 Not Found' error if the tag
-- doesn't exist.
deleteTagR :: TagPath -> Handler Value
deleteTagR path = do
    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    exists <- liftIO $ atomically $ do
        ui <- getUserIndex ii username currentTime
        mTag <- lookupTag ui path
        case mTag of Nothing  -> return False
                     Just tag -> removeTag ui tag >> return True

    if exists then sendResponseStatus noContent204 ()
              else notFound

-- -----------------------------------------------------------------------------

getStaticTag :: Tag -> STM StaticTag
getStaticTag Tag {..} = StaticTag tType <$> readTVar tSubTags
