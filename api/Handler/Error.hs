module Handler.Error (
      APIError (..), errorCode, errorMessage, errorHttpStatus, apiFail
    )
    where

import Prelude

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types.Status (
      Status, mkStatus, badRequest400, notFound404, unsupportedMediaType415
    )
import Yesod hiding (NotFound)

data APIError = BadRequest [Text]
              | NotFound
              | InvalidImage
              | IndexExhausted
    deriving Show

errorCode :: APIError -> Int
errorCode (BadRequest _) = 1
errorCode NotFound       = 2
errorCode InvalidImage   = 3
errorCode IndexExhausted = 4

errorName :: APIError -> Text
errorName (BadRequest _) = "BAD-REQUEST"
errorName NotFound       = "NOT-FOUND"
errorName InvalidImage   = "INVALID-IMAGE"
errorName IndexExhausted = "INDEX-EXHAUSTED"

errorMessage :: APIError -> Maybe Text
errorMessage (BadRequest errs) =
    let errsTxt = T.intercalate ", " errs
    in Just $! "The you submitted an inccorect request: " <> errsTxt
errorMessage NotFound =
    Just "The resource you are looking for doesn't exist"
errorMessage InvalidImage =
    Just "Unable to read the image format"
errorMessage IndexExhausted =
    Just "Your index has too many images"

errorHttpStatus :: APIError -> Status
errorHttpStatus (BadRequest _) = badRequest400
errorHttpStatus NotFound       = notFound404
errorHttpStatus InvalidImage   = unsupportedMediaType415
errorHttpStatus IndexExhausted = mkStatus 429 "Too Many Requests"

-- | Bypass remaining handler code and output the given error as JSON.
apiFail :: MonadHandler m => APIError -> m a
apiFail err = sendResponseStatus (errorHttpStatus err) (toJSON err)

instance ToJSON APIError where
    toJSON err =
        object [ "error" .= object [ "code"    .= errorCode    err
                                   , "name"    .= errorName    err
                                   , "message" .= errorMessage err ]
               ]
