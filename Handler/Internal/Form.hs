-- | Provides forms and field types for handlers.
module Handler.Internal.Form (
      filterForm, ImagesForm (..), imagesForm
    , imagesField, jsonField, scoreField, tagExpressionField
    ) where

import Import

import Control.Monad
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (decodeStrict', encode)
import Data.Conduit (($$), await)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Conduit (HttpException, parseUrl, responseBody)
import Network.HTTP.Client.Conduit (withResponse)
import Text.Parsec (parse)
import Text.Printf
import Vision.Image.Storage.DevIL (
      Autodetect (..), JPG (..), StorageError (UnknownFileType)
    , loadBS
    )

import qualified Control.Exception          as E
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Text                  as T

import Handler.Config (confMaxFileSize, confMinScore)
import Histogram (ResizedImage, resize)
import ObjectIndex (TagExpression, tagExpressionParser)

-- Forms -----------------------------------------------------------------------

filterForm :: (RenderMessage (HandlerSite m) FormMessage, Monad m)
           => FormInput m (Maybe TagExpression)
filterForm = iopt tagExpressionField "filter"

data ImagesForm = ImagesForm {
      ifImages     :: [(ResizedImage, Bool)]
    -- ^ True for each image which has been loaded as a JPG image.
    , ifIgnoreBack :: Bool
    , ifIgnoreSkin :: Bool
    }

imagesForm :: (MonadBaseControl IO m, MonadHandler m, HandlerSite m ~ App)
           => FormInput m ImagesForm
imagesForm = ImagesForm <$> ireq imagesField   "images"
                        <*> ireq checkBoxField "ignore_background"
                        <*> ireq checkBoxField "ignore_skin"

-- | Fields --------------------------------------------------------------------

data ImageError = UnreadableImage -- ^ Unable to read the image encoding.
                | TooLarge
                | InvalidUrl
                | NetworkError HttpException
                | OtherError

-- | Fails if at least one image is unreadable.
--
-- Resizes the image before returning.
imagesField :: (MonadBaseControl IO m, MonadHandler m, HandlerSite m ~ App)
            => Field m [(ResizedImage, Bool)]
            -- ^ Returns True for each image which has been loaded as a JPG
            -- image.
imagesField =
    Field {
          fieldParse = parser, fieldView = undefined, fieldEnctype = Multipart
        }
  where
    parser urls files = do
        eImgs <- runExceptT $
            (++) <$> readFiles files
                 <*> readUrls  urls

        return $! case eImgs of
            Right imgs@(_:_)      -> Right $! Just imgs
            Right []              -> Right Nothing
            Left UnreadableImage  -> Left "Unreadable image format"
            Left TooLarge         ->
                Left "The image exceed the maximum file size"
            Left InvalidUrl       -> Left "Invalid URL"
            Left (NetworkError e) ->
                let eText = T.pack $ show e
                    msg   = "Network error when downloading the image: "
                            <> eText
                in Left $ SomeMessage msg
            Left OtherError       ->
                Left "Unknown error while reading the image"

    -- Opens the images from the uploaded files.
    readFiles = mapM (readSource . fileSource)

    -- Downloads and opens the images from the URLs.
    readUrls urls = do
        manager <- httpManager <$> lift getYesod
        forM urls $ \url ->
            case parseUrl (T.unpack url) of
                Nothing  -> throwE InvalidUrl
                Just req ->
                    let onNetworkError = return . Left . NetworkError
                        readReq        = withResponse req $
                                            readSource . responseBody

                    in ExceptT $! liftIO $! E.handle onNetworkError $
                            runReaderT (runExceptT readReq) manager

    -- Consumes the ByteString stream up to the maximum file size.
    -- Throws an error if the stream is longer.
    sinkBsMaxSize maxFileSize
        | maxFileSize < 0 = lift $ lift $ throwE TooLarge
        | otherwise       = do
            mBs <- await
            case mBs of
                Nothing -> return []
                Just bs -> let len = BS.length bs
                           in (bs:) <$> sinkBsMaxSize (maxFileSize - len)

    -- Opens the image from a conduit source.
    readSource source = do
        bs <- runResourceT $ source $$ sinkBsMaxSize confMaxFileSize
        let bs' = BS.concat bs

        -- First tries to read the image as a JPG image.
        (img, isJPG) <- case loadBS JPG bs' of
            Right img            -> return (img, True)
            Left UnknownFileType ->
                -- If it fails, tries to autodetect the file type.
                case loadBS Autodetect bs' of
                    Right img -> return (img, False)
                    Left  _   -> throwE UnreadableImage
            Left _               -> throwE UnreadableImage

        let !resized = resize img
        return (resized, isJPG)

-- | Accepts an error message and returns a field which decode the JSON text
-- field into the corresponding required type.
jsonField :: (RenderMessage (HandlerSite m) FormMessage, FromJSON a, ToJSON a
             , Monad m)
          => Text -> Field m a
jsonField err =
    let jsonParser txt =
            case decodeStrict' (encodeUtf8 txt) of Just node -> Right node
                                                   Nothing   -> Left err
    in checkMap jsonParser (decodeUtf8 . BL.toStrict . encode) textField

scoreField :: (RenderMessage (HandlerSite m) FormMessage, Monad m)
           => Field m Float
scoreField =
    checkMap (cond . float) double doubleField
  where
    cond val | val >= confMinScore && val <= 1.0 = Right val
             | otherwise                         = Left msg

    msg = T.pack $ printf "Score must be a value between %f and 1.0"
                          confMinScore

tagExpressionField :: (RenderMessage (HandlerSite m) FormMessage, Monad m)
                   => Field m TagExpression
tagExpressionField =
    let parseTagExpression txt =
            case parse tagExpressionParser "" txt of
                Right expr -> Right expr
                Left _     -> Left "Invalid tag expression"
    in checkMap parseTagExpression (T.pack . show) textField

-- Same as check, but modifies the datatype.
--
-- In order to make this work, you must provide a function to convert back from
-- the new datatype to the old one (the second argument to this function).
checkMap :: Monad m
         => (a -> Either Text b) -> (b -> a) -> Field m a -> Field m b
checkMap f inv = checkMMap (return . f) inv

double :: Real a => a -> Double
double = realToFrac

float :: Real a => a -> Float
float = realToFrac
