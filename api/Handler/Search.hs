module Handler.Search (
      postColorSearchR, postImageSearchR
    ) where

import Import

import Control.Monad
import qualified Data.Vector as V

import Handler.Json ()

data ColorSearch w = ColorSearch {
      csColors :: [Color w]
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
    colorSearchForm = ColorSearch <$> ireq colorsField        "colors"
                                  <*> iopt tagExpressionField "filter"
                                  <*> iopt countField         "count"

    colorsField =
        let decodeColors expr =
                case decode' expr of
                    Just colors -> Right colors
                    Nothing     -> Left "Invalid colors expression"
        in check decodeColors textField

postImageSearchR :: Handler Value
postImageSearchR = undefined
