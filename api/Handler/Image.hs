module Handler.Image (
      getImagesR, postImagesR, getImageR, patchImageR, deleteImageR
    ) where

import Import
import Control.Monad
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Conduit (($$), runResourceT)
import Data.Conduit.Binary (sinkLbs)
import Data.Time.Clock (getCurrentTime)
import System.Random (newStdGen)
import qualified Vision.Image as I

import Handler.Json
import ImageIndex.Manage
import Util.Mashape

-- | Lists every image of the user.
getImagesR :: Handler Value
getImagesR = do
    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    Listing tagExpr count <- runInputGet listingForm

    imgs <- atomically $ do
        ui <- getUserIndex ii userName currentTime
        getMatchingImages ui tagExpr

    returnJson imgs

data NewImage = NewImage {
      niName       :: Maybe Text
    , niFiles      :: [FileInfo]
    , niTags       :: Maybe [TagPath]
    , niIgnoreBack :: Bool
    , niIgnoreSkin :: Bool
    }

-- | Registers a new image to the index. Returns a '201 Created' status on
-- success. Fails with a '400 Bad request' with an invalid query or a '429
-- Too Many Requests'.
postImagesR :: Handler Value
postImagesR =
    addImage <*> runInputPost newImageForm
  where
    newImageForm = NewImage <$> iopt textField     "name"
                            <*> ireq filesFiled    "images"
                            <*> iopt tagListField  "tags"
                            <*> ireq checkBoxField "ignore_background"
                            <*> ireq checkBoxField "ignore_skin"

    filesField = Field {
          fieldParse = \_ files ->
            if null files then return $! Right Nothing
                          else return $! Right files
        , fieldView = undefined, fieldEnctype = Multipart
        }

    tagListField =
        let decodeTagList expr =
                case decode' expr of
                    Just tags -> Right tags
                    Nothing   -> Left "Invalid tag list"
        in check decodeTagList textField

    addImage NewImage {..} = do
        mImgs <- readImages files

        case mImgs of
            Just imgs -> do
                let !hist = histogramsAverage $
                                map (compute ignoreBack ignoreSkin) imgs

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
                    ui    <- getUserIndex ii username currentTime
                    size  <- userIndexSize ui

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
            Nothing -> invalidArgs ["Unreadable image"]

    readImages files = runMaybeT $ do
        forM files $ \file -> do
            bs   <- liftIO $ runResourceT $ fileSourceRaw file $$ sinkLbs
            eImg <- liftIO $ I.loadBS Nothing bs

            case eImg of Left  _   -> MaybeT $! return Nothing
                         Right img -> return img

deleteImagesR :: Handler ()
deleteImagesR = do
    

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

    case mImg of
        Just img -> returnJson img
        Nothing  -> apiFail NotFound

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
              else apiFail NotFound

data Listing = Listing {
      lFilter :: Maybe TagExpression
    , lCount  :: Maybe Int
    }

listingForm =
    Listing <$> iopt tagExpressionField "filter"
            <*> iopt countField         "count"
  where
    countField = checkBool (> 0) "Non-positive count value" intField
