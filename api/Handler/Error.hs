module Handler.Error (APIError (..), errorCode, errorMessage, errorHttpStatus)
    where

import qualified Data.Text as T
import Network.HTTP.Types.Status (Status, badRequest400)
import Text.Parsec (ParseError)

data APIError = InvalidTagExpression ParseError
    deriving (Show, Enum)

errorCode :: APIError -> Int
errorCode = fromEnum

errorMessage :: APIError -> Maybe Text
errorMessage (InvalidTagExpression parseErr) =
   Just $! T.pack $ "Error when parsing the tag expression " ++ show parseErr

errorHttpStatus :: APIError -> Status
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
