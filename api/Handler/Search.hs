module Handler.Search (
      postColorSearchR, postImageSearchR
    ) where

import Import

import Control.Monad.STM (atomically)
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Time.Clock (getCurrentTime)

import Handler.Config (confDefaultMinScore)
import Handler.Error (APIError (IndexExhausted), apiFail)
import Handler.Internal.Form (
      ImagesForm (..), imagesForm, filterForm
    , jsonField, scoreField
    )
import Handler.Internal.Listing (listing, listingForm)
import Handler.Internal.Mashape (
      getMashapeHeaders, maxIndexSize, mhUser, mhSubscription
    )
import Handler.Internal.Json ()
import Handler.Internal.Type (SearchResult (..))
import Histogram (colorsHist, compareHist, computeHist, histsAvg)
import ImageIndex (
      IndexedHistogram, getMatchingImages, getUserIndex, iiHist, userIndexSize
    )

postColorSearchR :: Handler Value
postColorSearchR = do
    colors <- runInputPost (ireq colorsField "colors")
    search (colorsHist colors)
  where
    colorsField = jsonField "Invalid colors expression"

postImageSearchR :: Handler Value
postImageSearchR = do
    ImagesForm {..} <- runInputPost imagesForm
    search $ histsAvg $ map (computeHist ifIgnoreBack ifIgnoreSkin) ifImages

search :: IndexedHistogram -> Handler Value
search hist = do
    listingParams <- runInputPost listingForm
    tagExpr       <- runInputPost filterForm
    minScore      <- runInputPost (iopt scoreField "min_score")

    headers     <- getMashapeHeaders
    let username = mhUser headers
        maxSize  = maxIndexSize $ mhSubscription headers

    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    mImgs <- liftIO $ atomically $ do
        ui <- getUserIndex ii username currentTime
        size <- userIndexSize ui

        let indexIsFull = maybe False (size >) maxSize

        if indexIsFull then return Nothing
                       else Just <$> getMatchingImages ui tagExpr

    case mImgs of
        Just imgs -> do
            let minScore' = fromMaybe confDefaultMinScore minScore
                results   = [ SearchResult img score
                            | img <- S.toList imgs
                            , let score = compareHist hist (iiHist img)
                            , score >= minScore' ]
                !nResults = length results
                sorted    = sortBy (flip compare `on` srScore) results

            returnJson $ listing listingParams (Just nResults) sorted
        Nothing   -> apiFail IndexExhausted
