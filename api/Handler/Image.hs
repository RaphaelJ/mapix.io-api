module Handler.Image (
      getImagesR, postImagesR, getImageR, patchImageR, deleteImageR
    ) where

import Import
import Data.Conduit (($$), runResourceT)
import Data.Conduit.Binary (sinkLbs)
import Data.Time.Clock (getCurrentTime)
import System.Random (newStdGen)
import Vision.Image (StorageImage)
import qualified Vision.Image as I

import Handler.Json
import ImageIndex.Manage
import Util.Mashape

-- | Lists every image of the user.
getImagesR :: Handler Value
getImagesR = do
    Listing tagExpr count <- runInputGet listingForm

    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    imgs <- atomically $ do
        ui <- getUserIndex ii userName currentTime
        getMatchingImages ui tagExpr

    returnJson $ take (fromMaybe 100 count) imgs

data NewImage = NewImage {
      niName       :: Maybe Text
    , niImages     :: [StorageImage]
    , niTags       :: Maybe [TagPath]
    , niIgnoreBack :: Bool
    , niIgnoreSkin :: Bool
    }

-- | Registers a new image to the index. Returns a '201 Created' status on
-- success. Fails with a '400 Bad request' with an invalid query or a '429
-- Too Many Requests'.
postImagesR :: Handler Value
postImagesR = do
    NewImage {..} <- runInputPost newImageForm

    let !hist = histogramsAverage $
                    map (compute ignoreBack ignoreSkin) niImages

    headers     <- getMashapeHeaders
    let userName = mhUser headers
        maxSize  = maxIndexSize $ mhSubscription headers

    app         <- getYesod
    let key = encryptKey yesod
        ii  = imageIndex yesod

    currentTime <- lift getCurrentTime
    gen         <- lift newStdGen

    -- Tries to add the image. Returns Nothing if the index has too
    -- many images.
    mImg <- atomically $ do
        ui   <- getUserIndex ii username currentTime
        size <- userIndexSize ui

        if size < maxSize
            then do
                tags'    <- mapM (getTag ui) tags
                (img, _) <- addImage key ui gen name tags' hist
                touchUserIndex ii ui currentTime
                return $! Just img
            else return Nothing

    case mImg of
        Just img -> do
            url <- getUrlRender <*> pure (ImageR $! iCode img)
            addHeader "Location" url
            sendResponseStatus created201 (toJSON img)
        Nothing  -> apiFail IndexExhausted
  where
    newImageForm = NewImage <$> iopt textField     "name"
                            <*> ireq imagesField   "images"
                            <*> iopt tagListField  "tags"
                            <*> ireq checkBoxField "ignore_background"
                            <*> ireq checkBoxField "ignore_skin"

    tagListField = jsonField "Invalid tag list"

-- | Deletes every image matching the (optional) tag expression. Returns a '204
-- No Content'.
deleteImagesR :: Handler ()
deleteImagesR = do
    tagExpr <- runInputGet (iopt tagExpressionField "filter")

    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    atomically $ do
        ui   <- getUserIndex ii userName currentTime
        imgs <- getMatchingImages ui tagExpr
        mapM (removeImage ui) imgs

    sendResponseStatus noContent204 ()

-- | Returns the data associated with an image. Fails with a '404 Not found'
-- error when the image is not in the index.
getImageR :: ImageCode -> Handler Value
getImageR code = do
    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    mImg <- atomically $ do
        ui <- getUserIndex ii userName currentTime
        lookupImage ui code

    case mImg of Just img -> returnJson img
                 Nothing  -> notFound

-- | Returns a '204 No content' on success. Fails with a '404 Not found' error
-- when the image is not in the index.
deleteImageR :: ImageCode -> Handler ()
deleteImageR code = do
    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    exists <- atomically $ do
        ui <- getUserIndex ii userName currentTime
        mImg <- lookupImage ui code
        case mImg of Just img -> removeImage ui img >> return True
                     Nothing  -> return False

    if exists then sendResponseStatus noContent204 ()
              else notFound

data Listing = Listing {
      lFilter :: Maybe TagExpression
    , lCount  :: Maybe Int
    }

listingForm =
    Listing <$> iopt tagExpressionField "filter"
            <*> iopt countField         "count"
