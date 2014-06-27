module Handler.Error (APIError (..), errorCode, errorMessage, errorHttpStatus)
    where
import qualified Data.Text as T
import Network.HTTP.Types.Status (Status, badRequest400)
import Text.Parsec (ParseError)

data APIError = BadRequest [Text]
    deriving (Show, Enum)

errorCode :: APIError -> Int
errorCode = fromEnum

errorMessage :: APIError -> Maybe Text
errorMessage (BadRequest errs) =
    let errsTxt = T.intercalate "," errs
    in Just $! "The you submitted an inccorect request: " <> errsTxt

errorHttpStatus :: APIError -> Status
errorHttpStatus (BadRequest _) = badRequest400

-- | Bypass remaining handler code and output the given error as JSON.
apiFail :: APIError -> Handler a
apiFail err = sendResponseStatus (errorHttpStatus err) (returnJson err)

instance ToJson APIError where
    toJSON err =
        object [ "error" .= object $ [ "code"    .= errorCode err
                                     , "name"    .= T.pack $ show err
                                     , "message" .= errorMessage err ]
               ]
