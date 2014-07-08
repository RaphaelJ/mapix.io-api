module Handler.Search (
      postColorSearchR, postImageSearchR
    ) where

import Import

import Control.Monad
import qualified Data.Vector as V

import Handler.Json ()

postColorSearchR :: Handler Value
postColorSearchR = do
    colors                <- runInputPost (ireq colorsField "colors")
    Listing tagExpr count <- runInputPost listingForm

    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    imgs <- atomically $ do
        ui <- getUserIndex ii userName currentTime
        getMatchingImages ui tagExpr

    returnJson $ search count imgs
  where
    colorsField =
        let decodeColors expr =
                case decode' expr of
                    Just colors -> Right colors
                    Nothing     -> Left "Invalid colors expression"
        in check decodeColors textField

postImageSearchR :: Handler Value
postImageSearchR = undefined

search :: Set Image -> [(Image, Float)]
search count imgs =
    take $ sortBy $ 