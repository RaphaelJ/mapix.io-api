module Handler.Tag (getTagsR, getTagR, deleteTagR) where

import Import

import Control.Concurrent.STM (readTVar)
import Control.Monad
import Network.HTTP.Types.Status (noContent204)

import Handler.Internal.Mashape (getMashapeHeaders, mhUser)
import Handler.Internal.Json ()
import Handler.Internal.Type (StaticTag (..))
import ImageIndex (
      IndexSTM, Tag (..), TagPath
    , getUserIndex, liftSTM, lookupTag, removeTag, runTransaction
    , uiRootTag
    )
import qualified ImageIndex.Persistent as DB

-- Handlers --------------------------------------------------------------------

-- | Returns the hierarchy of tags.
getTagsR :: Handler Value
getTagsR = do
    username <- mhUser <$> getMashapeHeaders
    ii       <- imageIndex <$> getYesod

    rootTag <- runTransaction $ do
        ui <- getUserIndex ii username
        getStaticTag $ uiRootTag ui

    returnJson rootTag

-- | Returns the tag and its sub-tags. Fails with a '404 Not Found' error if the
-- tag doesn't exist.
getTagR :: TagPath -> Handler Value
getTagR path = do
    username <- mhUser <$> getMashapeHeaders
    ii       <- imageIndex <$> getYesod

    mTag <- runTransaction $ do
        ui   <- getUserIndex ii username
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

    exists <- runDB $ do
        exists <- runTransaction $ do
            ui <- getUserIndex ii username
            mTag <- lookupTag ui path
            case mTag of Nothing  -> return False
                         Just tag -> removeTag ui tag >> return True

        when exists $ do
            Entity userId _ <- DB.getUser username
            DB.removeTag userId path

        return exists

    if exists then sendResponseStatus noContent204 ()
              else notFound

-- -----------------------------------------------------------------------------

getStaticTag :: Tag -> IndexSTM StaticTag
getStaticTag Tag {..} = StaticTag tType <$> liftSTM (readTVar tSubTags)
