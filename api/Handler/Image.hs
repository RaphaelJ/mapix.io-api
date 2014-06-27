module Handler.Image (
      getImagesR, postImagesR, getImageR, patchImageR, deleteImageR
    ) where

import Import
import Data.Conduit (($$), runResourceT)
import Data.Conduit.Binary (sinkLbs)
import Data.Time.Clock (getCurrentTime)
import qualified Vision.Image as I

import ImageIndex.Manage
import Util.Mashape

-- | Lists every image of the user.
getImagesR :: Handler Value
getImagesR = do
    user        <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime
    tagExpr     <- getTagExpression

    imgs <- atomically $ do
        ui <- getUserIndex ii (userName user) currentTime
        getMatchingImages ui tagExpr

    returnJson imgs

data NewImage = NewImage {
      niFile       :: FileInfo
    , niName       :: Maybe Text
    , niTags       :: Maybe Text
    , niIgnoreBack :: Bool
    , niIgnoreSkin :: Bool
    }

-- | Registers a new image to the index. Returns a '201 Created' status on
-- success.
postImagesR :: Handler Value
postImagesR = do
    result <- runInputPostResult newImageForm

    case result of
        FormMissing      -> apiFail (BadRequest ["Missing request arguments"])
        FormFailure errs -> apiFail (BadRequest errs)
        FormSuccess img  ->
            case parseTagList (niTags img) of
                Right tags -> addImage (niFile img) (niName img) tags
                                       (niIgnoreBack img) (niIgnoreSkin img)
                Left  err  -> apiFail (BadRequest ["Invalid tag list"])
  where
    newImageForm = NewImage <$> ireq fileField     "file"
                            <*> iopt textField     "name"
                            <*> iopt textField     "tags"
                            <*> ireq checkBoxField "ignore_background"
                            <*> ireq checkBoxField "ignore_skin"

    parseTagList (Just tagList) = parse tagListParser "tag list" tagList
    parseTagList Nothing        = []

    addImage file name tags ignoreBack ignoreSkin = do
        bs   <- runResourceT $ fileSourceRaw file $$ sinkLbs
        eImg <- I.loadBS Nothing bs

        case eImg of
            Left  _   -> BadRequest ["Unable to read the image format"]
            Right img ->
                username    <- mhUser <$> getMashapeHeaders
                ii          <- imageIndex <$> getYesod
                currentTime <- lift getCurrentTime

                img <- atomically $ do
                    ui <- getUserIndex ii username currentTime

                sendResponseStatus (created201) (returnJson img)

getImageR :: Hmac -> Handler Value
getImageR = undefined

patchImageR :: Hmac -> Handler Value
patchImageR = undefined

deleteImageR :: Hmac -> Handler Value
deleteImageR = undefined
