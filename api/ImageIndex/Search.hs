module ImageIndex.Search (
      SearchResult (..), search
    ) where

import Prelude

import Data.Function
import Data.List
import Data.Set (Set)
import qualified Data.Set as S

import Histogram (compareHeterogeneous)
import ImageIndex.Type (
      IndexedImage (..), IndexedHistogram, IndexedHistogramBin
    )

type MatchScore = IndexedHistogramBin

data SearchResult = SearchResult {
      srImage :: IndexedImage
    , srScore :: MatchScore
    }

-- | Search the image set for images matching the histogram.
--
-- Returns the list of matched images by their decreasing matching score.
search :: MatchScore -> Set IndexedImage -> IndexedHistogram -> [SearchResult]
search !minScore imgs !hist =
    let results   = [ SearchResult img score
                    | img <- S.toList imgs
                    , let score = compareHeterogeneous hist (iiHist img)
                    , score >= minScore ]
    in sortBy (flip compare `on` srScore) results
