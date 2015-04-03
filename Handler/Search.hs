module Handler.Search (
      postColorSearchR, postImageSearchR
    ) where

import Import hiding (count)

import Data.Maybe

import Handler.Config (confDefaultCount, confMaxCount, confDefaultMinScore)
import Handler.Error (APIError (IndexExhausted), apiFail)
import Handler.Internal.Form (
      ImagesForm (..), imagesForm, filterForm
    , jsonField, scoreField
    )
import Handler.Internal.Listing (ListingForm (..), listing, listingForm)
import Handler.Internal.Mashape (
      getMashapeHeaders, maxIndexSize, mhUser, mhSubscription
    )
import Handler.Internal.Json ()
import Histogram (fromImages, fromColors)
import ObjectIndex (
      IndexedHistogram
    , getMatchingObjects, getUserIndex, runTransaction, search, touchUserIndex
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
    search' $ fromImages ifIgnoreBack ifIgnoreSkin (map fst ifImages)

search' :: IndexedHistogram -> Handler Value
search' hist = do
    listingParams <- runInputPost listingForm
    tagExpr       <- runInputPost filterForm
    minScore      <- runInputPost (iopt scoreField "min_score")

    headers <- getMashapeHeaders
    let username = mhUser headers
        maxSize  = maxIndexSize $ mhSubscription headers

    oi <- objectIndex <$> getYesod

    mObjs <- runTransaction $ do
        ui   <- getUserIndex oi username
        size <- userIndexSize ui

        let indexIsFull = maybe False (size >) maxSize

        if indexIsFull then return Nothing
                       else do touchUserIndex oi ui
                               Just <$> getMatchingObjects ui tagExpr

    case mObjs of
        Just objs ->
            let !offset    = fromMaybe 0 (lfOffset listingParams)
                !count     = maybe confDefaultCount (min confMaxCount)
                                   (lfCount listingParams)
                !nResults  = offset + count
                !minScore' = fromMaybe confDefaultMinScore minScore
                results    = search nResults minScore' objs hist
            in returnJson $ listing listingParams Nothing results
        Nothing   -> apiFail IndexExhausted
