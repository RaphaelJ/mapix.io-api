module Histogram.Compare (
      DirectIntersec (..), directIntersec,
    , compareHeterogeneous, comparePartial
    , compareCrossBinHist1D
    ) where

import Prelude

import Control.Lens
import Control.Monad.Trans.State.Strict
import Data.Function
import Foreign.Storable (Storable)
import Vision.Histogram (Histogram (..), compareIntersect, index, linearIndex)
import Vision.Primitive (Z, (:.), DIM1, DIM3, ix3, shapeLength)

import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS

import Histogram.Config (
      confCrossBinWeight, confHueBins, confHueSimilarityMatrix
    )
import Histogram.Type (
      BinsSimilarity (..), HeterogeneousHistogram (..), PartialHistogram (..)
    )

-- Types -----------------------------------------------------------------------

-- | The score of the intersection, between 0 (complete miss-match) and 1
-- (perfect match).
type Intersec = Weight

-- | Result of a simple (direct-bin) intersection of two histograms.
data DirectIntersec = DirectIntersec {
      diColors   :: Intersec
    , diGreys    :: Intersec
--     -- | Lowest intersection which will could be obtained once the cross-bin
--     -- intersection will be computed.
--     --
--     -- @
--     -- diMinScore direct ==   (diColors direct + minCrossScore direct)
--     --                      * hueCrossBinFactor
--     --                      + diGreys direct
--     -- @.
--     , diMinScore :: CrossIntersec
    } deriving Show

data CrossIntersec = CrossIntersec {
      ciColors      :: Intersec
    , ciCrossColors :: Intersec
    , ciGreys       :: Intersec
    } deriving Show

instance Ord CrossIntersec where
    compare = compare `on` intersec

-- Intersection ----------------------------------------------------------------

-- | Computes the simple (direct-bin) intersection of two histograms.
--
-- i.e. if the histograms H and H' have the symmetric similarity matrix M,
-- the direct-bin intersection will equal to:
--
-- @
-- sum [ min (H i) (H' i) | i <- bins ]
-- @
directIntersec :: HeterogeneousHistogram -> HeterogeneousHistogram
               -> DirectIntersec
directIntersec hist1 hist2 =
    -- TODO: Return 'Nothing' if it can ensure that the intersection will be
    -- smaller than a given minimum score.
    let intersecOn f = compareIntersect (f hist1) (f hist2)
    in DirectIntersec (intersecOn hhColors) (intersecOn hhGreys)

-- | Computes the cross-bin intersection of two histograms, given the already
-- computed direct-bin intersection.
--
-- The cross-bin intersection is the weighted sum of the direct-bin
-- intersections of histograms where the bins having a non-nul factor in the
-- similarity matrix have been merged.
--
-- i.e. the cross-bin intersection of two histograms H and H' having the
-- symmetric similarity matrix M is equal to:
--
-- @
-- sum [ M i j * (   [ min (H k) (H' k) | k <- bins, k /= i, k /= j ]
--                 + min (H i + H j) (H' i + H' j))
--     | i <- bins, j <- [i + 1..nBins]
--     ]
-- @
crossIntersec :: DirectIntersec
              -> HeterogeneousHistogram -> HeterogeneousHistogram
              -> CrossIntersec
crossIntersec DirectIntersec {..} (HeterogeneousHistogram _ colors1)
                                  (HeterogeneousHistogram _ colors2) =
    CrossIntersec diColors crossHues diGreys
  where
    -- Do a cross-bin comparison on the neighboring hues.
    crossHues = evalState 0 $ do
        -- TODO: Cross color comparison with the neighboring values and
        -- saturations.
        V.forM_ confHueSimilarityMatrix $ \BinsSimilarity {..} ->
            VS.forM_ (VS.enumFromN 0 nSats) $ \s ->
                VS.forM_ (VS.enumFromN 0 nVals) $ \v ->
                    let !bin1Hist1 = colors1 ! ix3 bsBin1 s v
                        !bin2Hist1 = colors1 ! ix3 bsBin2 s v
                        !bin1Hist2 = colors2 ! ix3 bsBin1 s v
                        !bin2Hist2 = colors2 ! ix3 bsBin2 s v
                    in id += bsSimilarity * (   diColors
                                              - min bin1Hist1 bin1Hist2
                                              - min bin2Hist1 bin2Hist2
                                              + min (bin1Hist1 bin2Hist1)
                                                    (bin1Hist2 bin2Hist2)

-- | Sums and normalizes the 'CrossIntersec' components to a value between 
-- 0 (complete miss-match) and 1 (perfect match).
intersec :: CrossIntersec -> Intersec
intersec CrossIntersec {..} =   (ciColors + ciCrossColors) * hueCrossBinFactor
                              + ciGreys

-- Functions -------------------------------------------------------------------

-- | Given the already computed direct intersection between two histograms,
-- returns the minimal intersection which could be computed with the cross-bin
-- comparison.
--
-- The minimal score of the cross-bin intersection is the direct intersection
-- times the sum of the weights in the similarity matrix.
--
-- i.e. if the histograms H and H' have the symmetric similarity matrix M,
-- the minimal weight that a bin will have in the cross-bin intersection will
-- equal:
--
-- @
-- sumWeights = minimum [ sum [ M i j | j <- bins, j > i ]
--                      | i <- bins
--                      ]
-- @
--
-- With a direct-bin intersection @direct@, the cross-bin intersection has the
-- value @direct * sumWeights@ as infimum.
minCrossIntersec :: DirectIntersec -> CrossIntersec
minCrossIntersec DirectIntersec {..} =
    CrossIntersec diColors minCrossColors diGreys
  where
    minCrossColors = diColors * hueCrossBinTotalWeight

-- | Returns 'True' if the first score can exceed the second one once the
-- cross-bin histogram will be computed.
canExceed :: DirectIntersec -> CrossIntersec -> Bool
DirectIntersec {..} `canExceed` cross =
    CrossIntersec diColors maxCrossColors diGreys > cross
  where
    -- TODO: Use min (sum grey1) (sum grey2) * hueCrossBinTotalWeight to
    -- estimates the maximum more accordin.
    maxCrossColors = hueCrossBinTotalWeight * (1.0 - diGreys)

-- -----------------------------------------------------------------------------

nHues :: Int
nHues = VS.length confHueBins

-- | The sum of the similarity factors of the similarity matrix.
hueCrossBinTotalWeight :: Weight
hueCrossBinTotalWeight = V.sum (V.map bsSimilarity confHueSimilarityMatrix)

-- | Number by which the color cross-bin intersection must be multiplied to
-- top at 1.0 in case of a perfect match.
hueCrossBinFactor :: Intersec
hueCrossBinFactor = 1 / (1 + hueCrossBinTotalWeight)

-- -- | Computes the intersection of two histograms.
-- --
-- -- Returns 'Nothing' if it can ensure that the intersection will be smaller than
-- -- the given minimum score.
-- compareHeterogeneous :: HeterogeneousHistogram -> HeterogeneousHistogram
--                      -> Maybe Weight
-- compareHeterogeneous minScore hist1 hist2 = do
--     let intersecOn         = compareIntersect . on
--         !intersecGrey      = intersecOn hhGreys hist1 hist2 * directBinWeight
--         !maxIntersecColors =    (min `on` hhColorsSum) hist1 hist2
--                               * directBinWeight
-- 
--     guard $! intersecGrey + maxIntersecColors + confCrossBinWeight >= minScore
-- 
--     let !intersecColors = intersecOn hhColors hist1 hist2 * directBinWeight
--         !intersec       = intersecGrey + intersecColors
-- 
--     guard $! intersec + confCrossBinWeight >= minScore
-- 
--     return $!   intersec
--               + compareCrossBinHist1D intersec (hhGreys hist1)  (hhGreys hist2)
--               + compareCrossBinHist3D intersec (hhColors hist1) (hhColors hist2)
-- 
--     let intersecOn         = compareIntersect . on
--         !intersecGrey      = intersecOn hhGreys hist1 hist2 * directBinWeight
--         !maxIntersecColors =    (min `on` hhColorsSum) hist1 hist2
--                               * directBinWeight
-- 
--     guard $! intersecGrey + maxIntersecColors + confCrossBinWeight >= minScore
-- 
--     let !intersecColors = intersecOn hhColors hist1 hist2 * directBinWeight
--         !intersec       = intersecGrey + intersecColors
-- 
--     guard $! intersec + confCrossBinWeight >= minScore
-- 
--     return $!   intersec
--               + compareCrossBinHist1D intersec (hhGreys hist1)  (hhGreys hist2)
--               + compareCrossBinHist3D intersec (hhColors hist1) (hhColors hist2)
-- 
-- comparePartial :: PartialHistogram -> HeterogeneousHistogram -> Weight
-- comparePartial PartialHistogram {..} HeterogeneousHistogram {..} =
--     -- TODO: Doesn't do any cross-bin comparison.
--       (   intersec hhColors phColorsIxs phColorsValues
--         + intersec hhGreys  phGreysIxs  phGreysValues)
--     / phSumValues
--   where
--     intersec hist ixs vals =
--         let step !ix !val = min val (hist `linearIndex` ix)
--         in VS.sum $ VS.zipWith step ixs vals
-- 
-- -- | Applies a pseudo cross-bin comparison of two histograms. Compare the sum
-- -- of each bin and its direct neighbors of the first histogram with the
-- -- corresponding sum in the second histogram.
-- --
-- -- @compareCrossBinHist1D intersec hist1 hist2@ where
-- -- @intersec == compareIntersect hist1 hist2@.
-- compareCrossBinHist1D :: Weight -> Histogram DIM1 Weight
--                       -> Histogram DIM1 Weight -> a
-- compareCrossBinHist1D intersec hist1@(Histogram sh1 vec1)
--                                hist2@(Histogram sh2 vec2)
--     | sh1 /= sh2 = error "Histograms are not of equal size."
--     | nBins <= 1 = intersec                                  -- [1]
--     | otherwise  =
--         -- TODO: Special cases for the first and the last bins.
--           (intersec + VS.sum (VS.izipWith step vec1 vec2) * crossBinFactor)
--         * dist1Normalize
--   where
--     !nBins = shapeLength sh1
--     !maxIx = nBins - 1
-- 
--     step ix v1 v2 =
--         let !hasLeft  = ix > 0
--             !hasRight = ix < maxIx
-- 
--         in if | hasLeft && hasRight      = f3 v1 v2 (ix - 1) (ix + 1)
--               | hasLeft                  = f2 v1 v2 (ix - 1)
--               | otherwise {- hasRight -} = f2 v1 v2 (ix + 1) -- Implied by [1].
-- 
--     f2 !v1 !v2 !ix =
--             min (hist1 `index` ix1 ix + v1) (hist2 `index` ix1 ix + v2)
--           / 2
-- 
--     f3 !v1 !v2 !leftIx !rightIx =
--             min (hist1 `index` ix1 leftIx + v1 + hist1 `index` ix1 rightIx)
--                 (hist2 `index` ix1 leftIx + v2 + hist2 `index` ix1 rightIx)
--           / 3
-- 
-- --     -- As we compared more bins in the cross-bin comparison, we need to
-- --     -- normalize the sum to the number of bin compared. Moreover the cross-bin
-- --     -- comparison is weighted differently from the direct-bin comparison.
-- --     !crossBinFactor =
-- --         let !nF2         = 2
-- --             !nF3         = realToFrac $! nBins - 2
-- --             !nCrossBins  = nF2 * 2 + nF3 * 3
-- --             !weightDist1 = confCrossBinWeight
-- --         in nBins / nCrossBins * weightDist1
--     !crossBinFactor = confCrossBinWeight
-- 
-- compareCrossBinHist3D :: Weight -> Histogram DIM3 Weight
--                       -> Histogram DIM3 Weight -> Weight
-- compareCrossBinHist3D intersec hist1@(Histogram sh1 vec1)
--                                hist2@(Histogram sh2 vec2)
--     | sh1 /= sh2 = error "Histograms are not of equal size."
--     | nBins <= 1 = intersec
--     | otherwise  =
--         let (!sum1D, !sum2D, !sum3D) = crossBinSums
--             
--         
--           (intersec + VU.sum (VU.izipWith step vec1 vec2) * crossBinFactor)
--         * dist1Normalize
--   where
--     !(Z :. nHues :. nSats :. nVals) = sh1
-- 
--     crossBinSums = evalState (0, 0, 0) $ do
--         let !maxHue = nHues - 1
--             !maxSat = nSats - 1
--             !maxVal = nVals - 1
-- 
--             -- Precomputes left and right indexes for every saturation and value
--             -- coordinates.
--             !satsIxs = V.generate nSats (nonCyclicalIxs maxSat)
--             !valsIxs = V.generate nVals (nonCyclicalIxs maxVal)
-- 
--         VS.forM_ (VS.enumFromN 0 nHues) $ \h -> do
--             let !hueIxs = cyclicalIxs maxHue h
-- 
--             V.forM_ (VS.enumFromN 0 nSats) $ \s -> do
--                 let !satIxs = satsIxs V.! s
-- 
--                 V.forM_ (VS.enumFromN 0 nSats) $ \v -> do
--                     let !valIxs  = valsIxs V.! v
--                         !center1 = hist1 `index` ix3 h s v
--                         !center2 = hist2 `index` ix3 h s v
-- 
--                         sum1D !hist =
--                                sumMapIx1 (\h' -> ix3 h' s v) hueIxs hist
--                              + sumMapIx1 (\s' -> ix3 h s' v) satIxs hist
--                              + sumMapIx1 (\v' -> ix3 h s v') valIxs hist
-- 
--                         !sum1D1 = sum1D hist1 + center1
--                         !sum1D2 = sum1D hist2 + center2
-- 
--                         sum2D !hist =
--                                sumMapIx2 (\h' s' -> ix3 h' s' v) hueIxs satIxs
--                                          hist
--                              + sumMapIx2 (\h' v' -> ix3 h' s v') hueIxs valIxs
--                                          hist
--                              + sumMapIx2 (\s' v' -> ix3 h s' v') satIxs valIxs
--                                          hist
-- 
--                         !sum2D1 = sum2D hist1 + sum1D1
--                         !sum2D2 = sum2D hist2 + sum1D2
-- 
--                         sum3D !hist = sumMapIx3 hueIxs satIxs valIxs hist
-- 
--                         !sum3D1 = sum3D hist1 + sum2D1
--                         !sum3D2 = sum3D hist2 + sum2D2
-- 
--                     _1 += min sum1D1 sum1D2
--                     _2 += min sum2D1 sum2D2
--                     _3 += min sum3D1 sum3D2
-- 
--     cyclicalIxs !maxIx !ix =
--         pair (if ix == 0     then maxIx else ix - 1)
--              (if ix == maxIx then 0     else ix + 1)
-- 
--     nonCyclicalIxs !maxIx !ix =
--         let !hasLeft  = ix > 0
--             !hasRight = ix < maxIx
--         in if | hasLeft   = if hasRight then pair (ix - 1) (ix + 1)
--                                         else VS.singleton (ix - 1)
--               | hasRight  = VS.singleton (ix + 1)
--               | otherwise = VS.empty
-- 
--     -- Sums the values at the indexes generated by the function iterated over
--     -- a vector.
--     -- > sumMapIx f vec1 hist = sum [ hist `index` f a | a <- vec ]
--     sumMapIx1 !f !vec !hist = VS.foldl' (\acc a -> acc + hist `index` f a) 0 vec
-- 
--     -- Sums the values at the indexes generated by the function iterated over
--     -- two vectors.
--     -- > sumMapIx2 f vec1 vec2 hist = sum [ hist `index` f a b
--     --                                    | a <- vec1, b <- vec2 ]
--     sumMapIx2 !f !vec1 !vec2 !hist =
--         VS.foldl' (\acc a -> acc + sumMapIx1 (f a) vec2 hist) 0 vec1
-- 
--     -- Sums the values at the indexes generated by the iteration over three 
--     -- vectors.
--     -- > sumMapIx3 vec1 vec2 vec3 hist = sum [ hist `index` ix3 a b c
--     --                                       | a <- vec1, b <- vec2, c <- vec3 ]
--     sumMapIx3 !f !vec1 !vec2 !vec3 !hist =
--         VS.foldl' (\acc a -> acc + sumMapIx2 (ix3 a) vec2 vec3 hist)
--                   0 vec1
-- 
--     nBins1D = 
--         let nHues = 3
-- 
--     -- Creates a two items vector.
--     pair a b = VS.cons a (VS.singleton b)

-- directBinWeight :: Fractional a => a
-- directBinWeight = 1 - confCrossBinWeight
-- 
-- -- The weight of cross-bin comparisons as compared to a direct bin comparison
-- -- (1.0).
-- 
-- weightDist1, weightDist2, weightDist3 :: Fractional a => a
-- weightDist1 = confCrossBinWeight
-- weightDist2 = confCrossBinWeight / sqrt(2) -- sqrt(2) ~ 1.41
-- weightDist3 = confCrossBinWeight / sqrt(3) -- sqrt(3) ~ 1.73
-- 
-- -- The factor we need to apply when we are doing cross-bin comparisons.
-- 
-- dist1Normalize1, dist1Normalize2, dist1Normalize3 :: Fractional a => a
-- dist1Normalize = 1 / (1 + weightDist1)
-- dist2Normalize = 1 / (1 + weightDist1 + weightDist2)
-- dist3Normalize = 1 / (1 + weightDist1 + weightDist2 + weightDist3)
