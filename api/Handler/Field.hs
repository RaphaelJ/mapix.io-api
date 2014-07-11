module Handler.Field (
      countField, imagesField
    ) where

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Error (Error, ErrorT (..), runErrorT)
import qualified Data.ByteString as S
import Data.Conduit (($$), ($$+-))
import Network.HTTP.Conduit (HttpException, 
import Network.HTTP.Types.Status (statusIsSuccessful)

import Handler.Config (Config (cMaxFileSize), defaultConfig)

countField = checkBool (> 0) "Non-positive count value" intField

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
          fieldParse = parse, fieldView = undefined, fieldEnctype = Multipart
        }
  where
    parse urls files = do
        eImgs <- runErrorT $ do
            readFiles files
            readUrls  urls

        case eImgs of
            Right (_:_)         -> Right mImgs
            Right []            -> Right Nothing
            Left Unreadable     -> Left "Unreadable image format"
            Left TooLarge       -> Left "The image exceed the maximum file size"
            Left InvalidUrl     -> Left "Invalid URL"
            Left NetworkError e ->
                Left $ "Network error when downloading the image: " ++ show e
            Left OtherError     -> Left "Unknown error while reading the image"

    -- Consumes the ByteString stream up to the maximum file size.
    -- Throws an error if the stream is longer.
    sinkLbsMaxSize maxFileSize
        | maxFileSize < 0 = throwError TooLarge
        | otherwise       = do
            mBs <- await
            case mBs of
                Nothing -> return []
                Just bs -> let len = S.length bs
                           in (bs:) <$> sinkLbsCheckMaxSize (maxFileSize - len)

    -- Downloads and opens the images from the URLs.
    readUrls urls = do
        manager <- httpManager <$> getYesod
        forM urls $ \url ->
            case parseUrl url of
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
    readFiles = mapM (readSource . fileSourceRaw)

    -- Opens the image from a conduit source.
    readSource source = do
        let maxFileSize = cMaxFileSize defaultConfig
        bs <- runResourceT $ source $$ sinkLbsMaxSize maxFileSize

        eImg <- liftIO $ I.loadBS Nothing bs

        case eImg of
            Left  _   -> throwError Unreadable
            Right img -> return img
