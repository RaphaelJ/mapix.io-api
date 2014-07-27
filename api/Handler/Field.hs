module Handler.Field (
      countField, imagesField, jsonField, tagExpressionField
    ) where

import Import

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Error (Error (..), ErrorT (..), runErrorT, throwError)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (decode')
import qualified Data.ByteString as S
import Data.Conduit (($$), ($$+-), await)
import qualified Data.Text as T
import Network.HTTP.Conduit (HttpException, http, parseUrl, responseBody)
import Network.HTTP.Client.Conduit (withResponse)
import Network.HTTP.Types.Status (statusIsSuccessful)
import qualified Vision.Image as I
import Text.Parsec (parse)
import Yesod

import Handler.Config (Config (cMaxFileSize), defaultConfig)
import ImageIndex.Tag (tagExpressionParser)

countField = checkBool (> 0) ("Non-positive count value" :: Text) intField

data ImageError = Unreadable -- ^ Unable to read the image encoding.
                | TooLarge
                | InvalidUrl
                | NetworkError HttpException
                | OtherError

instance Error ImageError where
    noMsg = OtherError

-- | Fails if at least one image is unreadable.
imagesField =
    Field {
          fieldParse = parser, fieldView = undefined, fieldEnctype = Multipart
        }
  where
    parser urls files = do
        eImgs <- runErrorT $ do
            readFiles files
            readUrls  urls

        case eImgs of
            Right (_:_)           -> Just <$> Right eImgs
            Right []              -> Right Nothing
            Left Unreadable       -> Left "Unreadable image format"
            Left TooLarge         ->
                Left "The image exceed the maximum file size"
            Left InvalidUrl       -> Left "Invalid URL"
            Left (NetworkError e) ->
                let eText = T.pack $ show e
                in Left $ "Network error when downloading the image: " <> eText
            Left OtherError       ->
                Left "Unknown error while reading the image"

    -- Consumes the ByteString stream up to the maximum file size.
    -- Throws an error if the stream is longer.
    sinkLbsMaxSize maxFileSize
        | maxFileSize < 0 = throwError TooLarge
        | otherwise       = do
            mBs <- await
            case mBs of
                Nothing -> return []
                Just bs -> let len = S.length bs
                           in (bs:) <$> sinkLbsMaxSize (maxFileSize - len)

    -- Downloads and opens the images from the URLs.
    readUrls urls = do
        manager <- httpManager <$> getYesod
        forM urls $ \url ->
            case parseUrl (T.unpack url) of
                Nothing  -> throwError InvalidUrl
                Just req -> do
                    res <- http req

                    let onNetworkError e = Left $! NetworkError e

                    res <- liftIO $
                        E.handle onNetworkError $
                            runErrorT $
                                runReaderT manager $
                                    withResponse req $ readSource . responseBody

                    ErrorT $! return res

    -- Opens the images from the uploaded files.
    readFiles = mapM (readSource . fileSource)

    -- Opens the image from a conduit source.
    readSource source = do
        let maxFileSize = cMaxFileSize defaultConfig
        bs <- runResourceT $ source $$ sinkLbsMaxSize maxFileSize

        eImg <- liftIO $ I.loadBS Nothing bs

        case eImg of
            Left  _   -> throwError Unreadable
            Right img -> return img

-- | Accepts an error message and returns a field which decode the JSON text
-- field into the corresponding required type.
jsonField err =
    let jsonParser expr = case decode' expr of Just node -> Right node
                                               Nothing   -> Left err
    in check jsonParser textField

scoreField = checkBool (\v -> v >= 0.0 && v <= 1.0)
                       "Score must be a value between 0.0 and 1.0"
                       doubleField

tagExpressionField =
    let parseTagExpression txt =
            case parse tagExpressionParser "" txt of
                Right expr -> Right expr
                Left _     -> Left "Invalid tag expression"
    in check parseTagExpression textField
