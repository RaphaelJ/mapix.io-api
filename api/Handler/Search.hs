module Handler.Search (
      postColorSearchR, postImageSearchR
    ) where

import Import

import Data.Maybe

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
import Histogram (fromImages, fromColors)
import ImageIndex (
      IndexedHistogram
    , getMatchingImages, getUserIndex, runTransaction, search, touchUserIndex
    , userIndexSize
    )

postColorSearchR :: Handler Value
postColorSearchR = do
    colors <- runInputPost (ireq colorsField "colors")
    search' $ fromColors colors
  where
    colorsField = jsonField "Invalid colors expression"

postImageSearchR :: Handler Value
postImageSearchR = do
    ImagesForm {..} <- runInputPost imagesForm
    search' $ fromImages ifIgnoreBack ifIgnoreSkin ifImages

search' :: IndexedHistogram -> Handler Value
search' hist = do
    listingParams <- runInputPost listingForm
    tagExpr       <- runInputPost filterForm
    minScore      <- runInputPost (iopt scoreField "min_score")

    headers <- getMashapeHeaders
    let username = mhUser headers
        maxSize  = maxIndexSize $ mhSubscription headers

    ii      <- imageIndex <$> getYesod

    mImgs <- runTransaction $ do
        ui   <- getUserIndex ii username
        size <- userIndexSize ui

        let indexIsFull = maybe False (size >) maxSize

        if indexIsFull then return Nothing
                       else do touchUserIndex ii ui
                               Just <$> getMatchingImages ui tagExpr

    case mImgs of
        Just imgs ->
            let minScore' = fromMaybe confDefaultMinScore minScore
                results   = search minScore' imgs hist
            in returnJson $ listing listingParams Nothing results
        Nothing   -> apiFail IndexExhausted
