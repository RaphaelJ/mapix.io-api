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

postColorSearchR :: Handler Value
postColorSearchR = do
    colors <- runInputPost (ireq colorsField "colors")
    search (colorsHist colors)
  where
    colorsField = jsonField "Invalid colors expression"

data ImageSearch = ImageSearch {
      isImages     :: [StorageImage]
    , isIgnoreBack :: Bool
    , isIgnoreSkin :: Bool
    }

postImageSearchR :: Handler Value
postImageSearchR = do
    ImageSearch imgs ignoreBack ignoreSkin <- runInputPost imageSearchForm
    search $ histsAvg $ map (compute ignoreBack ignoreSkin) imgs
  where
    imageSearchForm = ImageSearch <$> ireq imagesField   "image"
                                  <*> ireq checkBoxField "ignore_background"
                                  <*> ireq checkBoxField "ignore_skin"

data ResultLising = ResultLising {
      rlFilter :: Maybe TagExpression
    , rlCount  :: Maybe Int
    , rlMin    :: Maybe Double
    }

search :: Histogram DIM3 Float -> Handler Value
search hist = do
    ResultLising tagExpr count minScore <- runInputPost resultListingForm

    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    imgs <- atomically $ do
        ui <- getUserIndex ii userName currentTime
        getMatchingImages ui tagExpr

    let results = [ SearchResult img score
                  | img <- imgs
                  , let score = compareHist hist (hHist img)
                  , score >= minScore ]
        sorted  = sortBy (flip compare `on` srScore) results

    returnJson $ take (fromMaybe 100 count) sorted

resultListingForm = ResultLising <$> iopt tagExpressionField "filter"
                                 <*> iopt countField         "count"
                                 <*> iopt scoreField         "min_score"
