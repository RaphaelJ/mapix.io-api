module Handler.Search (
      postColorSearchR, postImageSearchR
    ) where

import Import

import Control.Monad
import Data.Maybe
import qualified Data.Vector as V

import Handler.Json ()
import Histogram.Color (colorsHist)
import Histogram.Compare (compareHist)
import Histogram.Compute ()

data ResultLising = ResultLising {
      rlFilter :: Maybe TagExpression
    , rlCount  :: Maybe Int
    , rlMin    :: Maybe Double
    }

data SearchResult = SearchResult {
      srImage :: Image
    , srScore :: Float
    }

postColorSearchR :: Handler Value
postColorSearchR = do
    colors <- runInputPost (ireq colorsField "colors")
    ResultLising tagExpr count minScore <- runInputPost resultListingForm

    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    imgs <- atomically $ do
        ui <- getUserIndex ii userName currentTime
        getMatchingImages ui tagExpr

    let !hist = colorsHist colors

    returnJson $ take (fromMaybe 100 count) $ search hist imgs
  where
    colorsField = jsonField "Invalid colors expression"

postImageSearchR :: Handler Value
postImageSearchR = undefined

search :: Histogram DIM3 Float -> Set Image -> [(Image, Double)]
search hist imgs =
    let results = [ SearchResult img (compareHist hist (hHist img))
                  | img <- imgs ]
    in sortBy (flip compare `on` snd) results

resultListingForm = ResultLising <$> iopt tagExpressionField "filter"
                                 <*> iopt countField         "count"
                                 <*> iopt scoreField         "min_score"

scoreField = checkBool (\v -> v >= 0.0 && v <= 1.0)
                       "Score must be a value between 0.0 and 1.0"
                       doubleField
