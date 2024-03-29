module ObjectIndex.Search (
      SearchResult (..), search
    ) where

import ClassyPrelude


import Histogram (
      Intersec, directIntersec, intersec
    , minIntersec, canExceed
    )
import ObjectIndex.Type (IndexedObject (..), IndexedHistogram)

data SearchResult a = SearchResult {
      srObject :: IndexedObject
    , srScore  :: a
    }

-- | Search the object set for objects matching the histogram.
--
-- Returns the list of matched objects by their decreasing matching score.
search :: Int -> Intersec -> Set IndexedObject -> IndexedHistogram
       -> [SearchResult Intersec]
search !nResults !minScore !objs !hist =
    let -- Sorts the objects by their direct-bin intersection score.
        directScores = sortBy (compare `on` (minIntersec . srScore))
                              [ SearchResult obj (directIntersec hist ioHist)
                              | obj@(IndexedObject {..}) <- toList objs
                              ]

        -- Removes objects whose cross-bin intersection score couldn't exceed
        -- minScore or the nResults-th direct-bin intersection score.
        !minScore' | length objs > nResults =
                        let lastScore = indexEx directScores (nResults - 1)
                        in max minScore (minIntersec (srScore lastScore))
                   | otherwise              = minScore

        directScores' = takeWhile ((`canExceed` minScore') . srScore)
                                  directScores

        crossScores = [ direct { srScore = score }
                      | direct@(SearchResult {..}) <- directScores'
                      , let !IndexedObject {..} = srObject
                            !score = intersec srScore hist ioHist
                      , score >= minScore'
                      ]

    in take nResults $ sortBy (compare `on` srScore) crossScores
