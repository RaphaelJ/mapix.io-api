module Handler.Tag (getTagsR, getTagR, deleteTagR) where

import Import

import Prelude

import Control.Concurrent.STM (atomically)
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (fromText, toLazyText)
import Data.Time.Clock (getCurrentTime)

import ImageIndex.Manage (getUserIndex, getTagImages)
import ImageIndex.Type
import Util.Mashape (getMashapeHeaders, mhUser)

-- Handlers --------------------------------------------------------------------

-- | Returns the hierarchy of tags.
getTagsR :: Handler Html
getTagsR = do
    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    rootTag <- atomically $ do
        ui <- getUserIndex ii username currentTime
        uiRootTag ui

    returnJson rootTag

-- | Returns the tag and its sub-tags. Fails with a '404 Not Found' error if the
-- tag doesn't exist.
getTagR :: TagPath -> Handler Html
getTagR path = do
    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    mTag <- atomically $ do
        ui <- getUserIndex ii username currentTime
        lookupTag ui path

    case mTag of Just tag -> returnJson rootTag
                 _        -> notFound

-- | Removes this tag and its subtags without removing the images. Returns a
-- '204 No content' on success. Fails with a '404 Not Found' error if the tag
-- doesn't exist.
deleteTagR :: TagPath -> Handler Html
deleteTagR path = do
    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    exists <- atomically $ do
        ui <- getUserIndex ii username currentTime
        mTag <- lookupTag ui path
        case mTag of Nothing  -> return False
                     Just tag -> removeTag tag >> return True

    if exists then sendResponseStatus noContent204 ()
              else notFound
