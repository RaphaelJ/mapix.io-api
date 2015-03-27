module Handler.Error (
      APIError (..), errorCode, errorMessage, errorHttpStatus, apiFail
    )
    where

import Prelude

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Status (
      Status (statusCode), mkStatus, badRequest400, notFound404
    , methodNotAllowed405, unsupportedMediaType415, internalServerError500
    )
import Yesod hiding (NotFound)

data APIError = BadRequest [Text]
              | NotFound
              | MethodNotAllowed Method
              | InvalidImage
              | IndexExhausted
              | InternalServerError Text
    deriving Show

errorCode :: APIError -> Int
errorCode = statusCode . errorHttpStatus

errorName :: APIError -> Text
errorName (BadRequest _)          = "BAD-REQUEST"
errorName NotFound                = "NOT-FOUND"
errorName (MethodNotAllowed _)    = "METHOD-NOT-ALLOWED"
errorName InvalidImage            = "INVALID-IMAGE"
errorName IndexExhausted          = "INDEX-EXHAUSTED"
errorName (InternalServerError _) = "INTERNAL-SERVER-ERROR"

errorMessage :: APIError -> Maybe Text
errorMessage (BadRequest msgs) =
    let msgsTxt = T.intercalate ", " msgs
    in Just $ "You submitted an incorrect request: " <> msgsTxt
errorMessage NotFound =
    Just "The resource you are looking for doesn't exist"
errorMessage (MethodNotAllowed method) =
    Just $ "Method not allowed: " <> decodeUtf8 method
errorMessage InvalidImage =
    Just "Unable to read the image format"
errorMessage IndexExhausted =
    Just "Your index has too many objects"
errorMessage (InternalServerError msg) =
    Just $ "An unexpected error occurred during the execution of your request: "
           <> msg

errorHttpStatus :: APIError -> Status
errorHttpStatus (BadRequest _)          = badRequest400
errorHttpStatus NotFound                = notFound404
errorHttpStatus (MethodNotAllowed _)    = methodNotAllowed405
errorHttpStatus InvalidImage            = unsupportedMediaType415
errorHttpStatus IndexExhausted          = mkStatus 429 "Too Many Requests"
errorHttpStatus (InternalServerError _) = internalServerError500

-- | Bypass remaining handler code and output the given error as JSON.
apiFail :: MonadHandler m => APIError -> m a
apiFail err = sendResponseStatus (errorHttpStatus err) (toJSON err)

instance ToJSON APIError where
    toJSON err =
        object [ "error" .= object [ "code"    .= errorCode    err
                                   , "name"    .= errorName    err
                                   , "message" .= errorMessage err ]
               ]
