module ImageIndex.Search (
      SearchResult (..), search
    ) where

import Prelude

import Data.Function
import Data.List
import Data.Set (Set)
import qualified Data.Set as S

import Histogram (
      Intersec, directIntersec, crossIntersec, intersec
    , minIntersec, canExceed
    )
import ImageIndex.Type (IndexedImage (..), IndexedHistogram)

data SearchResult a = SearchResult {
      srImage :: IndexedImage
    , srScore :: a
    }

-- | Search the image set for images matching the histogram.
--
-- Returns the list of matched images by their decreasing matching score.
search :: Int -> Intersec -> Set IndexedImage -> IndexedHistogram
       -> [SearchResult Intersec]
search !nResults !minScore imgs !hist =
    let -- Sorts the images by their direct-bin intersection score.
        directScores = sortBy (compare `on` (minIntersec . srScore))
                              [ SearchResult img (directIntersec hist iiHist)
                              | img@(IndexedImage {..}) <- S.toList imgs
                              ]

        -- Removes images whose cross-bin intersection score couldn't exceed
        -- minScore or the nResults-th direct-bin intersection score.
        !minScore' | S.size imgs > nResults =
                        let lastScore = directScores !! (nResults - 1)
                        in max minScore (minIntersec (srScore lastScore))
                   | otherwise              = minScore

        directScores' = takeWhile ((`canExceed` minScore') . srScore)
                                  directScores

        crossScores = [ direct { srScore = score }
                      | direct@(SearchResult {..}) <- directScores'
                      , let !IndexedImage {..} = srImage
                            !crossScore = crossIntersec srScore hist iiHist
                            !score      = intersec crossScore
                      , score >= minScore'
                      ]

    in take nResults $ sortBy (compare `on` srScore) crossScores
