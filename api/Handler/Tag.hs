module Handler.Tag (getTagsR, getTagR, deleteTagR) where

import Import

import Control.Concurrent.STM (readTVar)
import Network.HTTP.Types.Status (noContent204)

import Handler.Internal.Mashape (getMashapeHeaders, mhUser)
import Handler.Internal.Json ()
import Handler.Internal.Type (StaticTag (..))
import ObjectIndex (
      IndexSTM, Tag (..), TagPath
    , getUserIndex, liftSTM, lookupTag, removeTag, runTransaction
    , uiRootTag
    )

-- Handlers --------------------------------------------------------------------

-- | Returns the hierarchy of tags.
getTagsR :: Handler Value
getTagsR = do
    username <- mhUser <$> getMashapeHeaders
    oi       <- objectIndex <$> getYesod

    rootTag <- runTransaction $ do
        ui <- getUserIndex oi username
        getStaticTag $ uiRootTag ui

    returnJson rootTag

-- | Returns the tag and its sub-tags.
--
-- Fails with a '404 Not Found' error if the tag doesn't exist.
getTagR :: TagPath -> Handler Value
getTagR path = do
    username <- mhUser <$> getMashapeHeaders
    oi       <- objectIndex <$> getYesod

    mTag <- runTransaction $ do
        ui   <- getUserIndex oi username
        mTag <- lookupTag ui path
        case mTag of Just tag -> Just <$> getStaticTag tag
                     Nothing  -> return Nothing

    maybe notFound returnJson mTag

-- | Removes this tag and its subtags without removing the objects.
--
-- Returns a '204 No content' on success. Fails with a '404 Not Found' error if
-- the tag doesn't exist.
deleteTagR :: TagPath -> Handler Value
deleteTagR path = do
    username    <- mhUser <$> getMashapeHeaders
    oi          <- objectIndex <$> getYesod

    exists <- runTransaction $ do
        ui <- getUserIndex oi username
        mTag <- lookupTag ui path
        case mTag of Nothing  -> return False
                     Just tag -> removeTag ui tag >> return True

    if exists then sendResponseStatus noContent204 ()
              else notFound

-- -----------------------------------------------------------------------------

getStaticTag :: Tag -> IndexSTM StaticTag
getStaticTag Tag {..} = StaticTag tType <$> liftSTM (readTVar tSubTags)
