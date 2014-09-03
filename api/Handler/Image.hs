module Handler.Image (
      getImagesR, postImagesR, deleteImagesR
    , getImageR, deleteImageR
    ) where

import Import

import Control.Monad.STM (atomically)
import qualified Data.Foldable as F
import Data.Maybe
import Data.Time.Clock (getCurrentTime)
import qualified Data.Set as S
import Network.HTTP.Types.Status (created201, noContent204)
import System.Random (newStdGen)
import Vision.Image (StorageImage)

import Handler.Error (APIError (IndexExhausted), apiFail)
import Handler.Internal.Form (
      filterForm, imagesField, jsonField, tagExpressionField
    )
import Handler.Internal.Json ()
import Handler.Internal.Listing (listing, listingForm)
import Handler.Internal.Mashape (
      getMashapeHeaders, maxIndexSize, mhSubscription, mhUser
    )
import Handler.Internal.Type (ImageWithColors (..))
import ImageIndex (
      ImageCode, IndexedImage (..), TagPath
    , getMatchingImages, getTag, getUserIndex, lookupImage, newImage
    , removeImage, touchUserIndex, userIndexSize
    )
import Histogram (fromImages)

-- | Lists every image of the user.
getImagesR :: Handler Value
getImagesR = do
    listingParams <- runInputGet listingForm
    tagExpr       <- runInputGet filterForm

    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    imgs <- liftIO $ atomically $ do
        ui <- getUserIndex ii username currentTime
        getMatchingImages ui tagExpr

    returnJson $ listing listingParams (Just $! S.size imgs) (S.toList imgs)

data NewImage = NewImage {
      niName       :: Maybe Text
    , niImages     :: [StorageImage]
    , niTags       :: Maybe [TagPath]
    , niIgnoreBack :: Bool
    , niIgnoreSkin :: Bool
    }

-- | Registers a new image to the index.
--
-- Returns a '201 Created' status on success. Fails with a '400 Bad request'
-- with an invalid query or a '429 Too Many Requests'.
postImagesR :: Handler Value
postImagesR = do
    NewImage {..} <- runInputPost newImageForm

    let !hist = fromImages niIgnoreBack niIgnoreSkin niImages
    liftIO $ print hist

    headers     <- getMashapeHeaders
    let username = mhUser headers
        maxSize  = maxIndexSize $ mhSubscription headers

    app         <- getYesod
    let key = encryptKey app
        ii  = imageIndex app

    currentTime <- lift getCurrentTime
    gen         <- lift newStdGen

    -- Tries to add the image. Returns Nothing if the index has too
    -- many images.
    mImg <- liftIO $ atomically $ do
        ui   <- getUserIndex ii username currentTime
        size <- userIndexSize ui

        let indexIsFull = maybe False (size >=) maxSize

        if indexIsFull
            then return Nothing
            else do
                tags     <- mapM (getTag ui) (fromMaybe [] niTags)
                (img, _) <- newImage key ui gen niName tags hist
                touchUserIndex ii ui currentTime
                return $! Just img

    case mImg of
        Just img -> do
            url <- getUrlRender <*> pure (ImageR $! iiCode img)
            addHeader "Location" url
            sendResponseStatus created201 (toJSON $ ImageWithColors img)
        Nothing  -> apiFail IndexExhausted
  where
    newImageForm = NewImage <$> iopt textField     "name"
                            <*> ireq imagesField   "image"
                            <*> iopt tagListField  "tags"
                            <*> ireq checkBoxField "ignore_background"
                            <*> ireq checkBoxField "ignore_skin"

    tagListField = jsonField "Invalid tag list"

-- | Deletes every image matching the (optional) tag expression.
--
-- Returns a '204 No Content'.
deleteImagesR :: Handler ()
deleteImagesR = do
    tagExpr <- runInputGet (iopt tagExpressionField "filter")

    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    liftIO $ atomically $ do
        ui   <- getUserIndex ii username currentTime
        imgs <- getMatchingImages ui tagExpr
        F.mapM_ (removeImage ui) imgs

    sendResponseStatus noContent204 ()

-- | Returns the data associated with an image.
--
-- Fails with a '404 Not found' error when the image is not in the index.
getImageR :: ImageCode -> Handler Value
getImageR code = do
    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    mImg <- liftIO $ atomically $ do
        ui <- getUserIndex ii username currentTime
        lookupImage ui code

    case mImg of Just img -> returnJson $ ImageWithColors img
                 Nothing  -> notFound

-- | Returns a '204 No content' on success. Fails with a '404 Not found' error
-- when the image is not in the index.
deleteImageR :: ImageCode -> Handler ()
deleteImageR code = do
    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    exists <- liftIO $ atomically $ do
        ui <- getUserIndex ii username currentTime
        mImg <- lookupImage ui code
        case mImg of Just img -> removeImage ui img >> return True
                     Nothing  -> return False

    if exists then sendResponseStatus noContent204 ()
              else notFound
