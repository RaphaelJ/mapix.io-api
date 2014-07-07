module Handler.Search (
      postColorSearchR, postImageSearchR
    ) where

import Import

data ColorSearch = ColorSearch {
      csColors :: Text
    , csFilter :: Maybe Text
    , csCount  :: Maybe Int
    }

postColorSearchR :: Handler Value
postColorSearchR = do
    result <- runInputPostResult colorSearchForm

    case result of
        FormMissing      -> apiFail (BadRequest ["Missing request arguments"])
        FormFailure errs -> apiFail (BadRequest errs)
        FormSuccess req  ->
            case parseTagExpr (csFilter req) of
                Right expr -> do
                    case search (csColors)
                Left  err  -> apiFail (BadRequest ["Invalid tag expression"])
  where
    colorSearchForm = ColorSearch <$> ireq textField "colors"
                                  <*> iopt textField "filter"
                                  <*> iopt intField  "count"

    colorField = textField {
          fieldParse = \vals files ->
                case fieldParse textField vals files of
                    Right expr ->
                        case decode' expr of
                            Just    -> Right 
                            Nothing -> Left "Invalid color expression"
                    Left err   -> Left err
        }

postImageSearchR :: Handler Value
postImageSearchR = undefined
