module Handler.Error (APIError (..), errorCode, errorMessage, errorHttpStatus)
    where
import qualified Data.Text as T
import Network.HTTP.Types.Status (Status, badRequest400)
import Text.Parsec (ParseError)

data APIError = EmptyFormException
              | InvalidFormException [Text]
              | InvalidTagExpression ParseError
    deriving (Show, Enum)

errorCode :: APIError -> Int
errorCode = fromEnum

errorMessage :: APIError -> Maybe Text
errorMessage EmptyFormException              =
    Just "You submitted an empty form."
errorMessage (MissingFormException errs)     =
    let errsTxt = T.intercalate "," errs
    in Just $! "The you submitted incorrectly formatted data: " <> errsTxt
errorMessage (InvalidTagExpression err) =
    let errTxt = T.pack $ show parseErr
    in Just $! "Error when parsing the tag expression: " <> errTxt

errorHttpStatus :: APIError -> Status
errorHttpStatus EmptyFormException       = badRequest400
errorHttpStatus InvalidTagExpression     = badRequest400
errorHttpStatus (InvalidTagExpression _) = badRequest400

-- | Bypass remaining handler code and output the given error as JSON.
apiFail :: APIError -> Handler a
apiFail err = sendResponseStatus (errorHttpStatus err) (returnJson err)

instance ToJson APIError where
    toJSON err =
        object [ "error" .= object $ [ "code"    .= errorCode err
                                     , "name"    .= T.pack $ show err
                                     , "message" .= errorMessage err ]
               ]
