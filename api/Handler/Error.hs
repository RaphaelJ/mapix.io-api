module Handler.Error (APIError (..), errorCode, errorMessage, errorHttpStatus)
    where

import Network.HTTP.Types.Status (Status)

data APIError = InvalidTagExpression
    deriving (Show, Enum)

errorCode :: APIError -> Int
errorCode = fromEnum

errorMessage :: APIError -> Maybe Text
errorMessage (InvalidTagExpression parseErr) =


errorHttpStatus :: APIError -> Status
errorHttpStatus =

-- | Bypass remaining handler code and output the given error as JSON.
apiFail :: APIError -> Handler a
apiFail err = sendResponseStatus (errorHttpStatus err) (returnJson err)

instance ToJson APIError where
    toJSON err =
        object [ "error" . =
              object $ [ "code"    .= errorCode err
                       , "name"    .= T.pack $ show err
                       , "message" .= errorMessage err ]
               ]
