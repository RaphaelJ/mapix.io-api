module ImageIndex.Search (
      SearchResult (..), search
    ) where

import Prelude

import Data.Function
import Data.List
import Data.Set (Set)
import qualified Data.Set as S

import Histogram (Weight, compareHeterogeneous)
import ImageIndex.Type (IndexedImage (..), IndexedHistogram)

data SearchResult a = SearchResult {
      srImage :: IndexedImage
    , srScore :: a
    }

-- | Search the image set for images matching the histogram.
--
-- Returns the list of matched images by their decreasing matching score.
search :: Int -> Weight -> Set IndexedImage -> IndexedHistogram
       -> [SearchResult]
search !nResults !minScore imgs !hist =
    let directs = sortByScore directs  [
              SearchResult img directScore
            | img@(IndexedImage {..}) <- S.toList imgs
            , let !directScore = intersec hist iiHist
            ]
                  , let !score = crossIntersec (intersec hist iiHist) hist
                                               iiHist
                  , score >= minScore ]
        !lastDirects = directs !! (nResults - 1)
    in [ SearchResult img (crossIntersec srScore hist iiHist
        | SearchResult {..} <- takeWhile (`canExceed` lastDirect) directs
    ]
  where
    sortByScore = flip compare `on` srScore

--     let results = [ SearchResult img score
--                   | IndexedImage {..} <- S.toList imgs
--                   , let !score = crossIntersec (intersec hist iiHist) hist
--                                                iiHist
--                   , score >= minScore ]
--     in sortBy (flip compare `on` srScore) results
    
    
