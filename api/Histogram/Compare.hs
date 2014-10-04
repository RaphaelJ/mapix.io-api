module Histogram.Compare (
      compareHeterogeneous, comparePartial
    , compareCrossBinHist1D
    ) where

import Prelude

import Data.Function
import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable)
import Vision.Histogram (Histogram (..), compareIntersect, index, linearIndex)
import Vision.Primitive (Z, (:.), DIM1, DIM3, ix1, shapeLength)

import Histogram.Config (confCrossBinWeight)
import Histogram.Type (HeterogeneousHistogram (..), PartialHistogram (..))

-- | Computes the intersection of two histograms.
--
-- Returns 'Nothing' if it can ensure that the intersection will be smaller than
-- the given minimum score.
compareHeterogeneous :: (Ord a, Num a, Storable a)
                     => a -> HeterogeneousHistogram a
                     -> HeterogeneousHistogram a -> Maybe a
compareHeterogeneous minScore hist1 hist2 = do
    let intersecOn         = compareIntersect . on
        !intersecGrey      = intersecOn hhGreys hist1 hist2 * directBinWeight
        !maxIntersecColors =    (min `on` hhColorsSum) hist1 hist2
                              * directBinWeight

    guard $! intersecGrey + maxIntersecColors + confCrossBinWeight >= 1.0

    let !intersecColors = compareOn hhColors hist1 hist2 * directBinWeight

    guard $! intersecGrey + intersecColors    + confCrossBinWeight >= 1.0

    return $!   intersecGrey + intersecColors
              + compareCrossBinHist1D + compareCrossBinHist3D
{-# SPECIALIZE compareHeterogeneous :: Float -> HeterogeneousHistogram Float
                                    -> HeterogeneousHistogram Float -> Float #-}

comparePartial :: (Storable a, Ord a, Fractional a)
               => PartialHistogram a -> HeterogeneousHistogram a -> a
comparePartial PartialHistogram {..} HeterogeneousHistogram {..} =
      (   intersec hhColors phColorsIxs phColorsValues
        + intersec hhGreys  phGreysIxs  phGreysValues)
    / phSumValues
  where
    intersec hist ixs vals =
        let step !ix !val = min val (hist `linearIndex` ix)
        in V.sum $ V.zipWith step ixs vals
{-# SPECIALIZE comparePartial :: PartialHistogram Float
                              -> HeterogeneousHistogram Float -> Float #-}

-- | Applies a pseudo cross-bin comparison of two histograms. Compare the sum
-- of each bin and its direct neighbors of the first histogram with the
-- corresponding sum in the second histogram.
--
-- @compareCrossBinHist1D intersec hist1 hist2@ where
-- @intersec == compareIntersect hist1 hist2@.
compareCrossBinHist1D :: (Storable a, Ord a, Fractional a)
              => a -> Histogram DIM1 a -> Histogram DIM1 a -> a
compareCrossBinHist1D intersec hist1@(Histogram sh1 vec1)
                               hist2@(Histogram sh2 vec2)
    | sh1 /= sh2 = error "Histograms are not of equal size."
    | nBins <= 1 = intersec                                  -- [1]
    | otherwise  =
          (intersec + V.sum (V.izipWith step vec1 vec2) * crossBinFactor)
        * dist1Normalize
  where
    !nBins       = shapeLength sh1
    !maxIx       = nBins - 1

    step ix v1 v2 =
        let !hasLeft  = ix > 0
            !hasRight = ix < maxIx

        in if | hasLeft && hasRight      = f3 v1 v2 (ix - 1) (ix + 1)
              | hasLeft                  = f2 v1 v2 (ix - 1)
              | otherwise {- hasRight -} = f2 v1 v2 (ix + 1) -- Implied by [1].

    f2 !v1 !v2 !ix =
          min (hist1 `index` ix1 ix + v1) (hist2 `index` ix1 ix + v2)

    f3 !v1 !v2 !leftIx !rightIx =
          min (hist1 `index` ix1 leftIx + v1 + hist1 `index` ix1 rightIx)
              (hist2 `index` ix1 leftIx + v2 + hist2 `index` ix1 rightIx)

    -- As we compared more bins in the cross-bin comparison, we need to
    -- normalize the sum to the number of bin compared. Moreover the cross-bin
    -- comparison is weighted differently from the direct-bin comparison.
    !crossBinFactor =
        let !nF2         = 2
            !nF3         = realToFrac $! nBins - 2
            !nCrossBins  = nF2 * 2 + nF3 * 3
            !weightDist1 = confCrossBinWeight
        in nCrossBins / nBins * weightDist1
{-# SPECIALIZE compareCrossBinHist1D :: Float -> Histogram DIM1 Float
                                     -> Histogram DIM1 Float -> Float #-}

compareHist3D :: (Storable a, Ord a, Fractional a)
              => Histogram DIM3 a -> Histogram DIM3 a -> a
compareHist3D hist1@(Histogram sh1 vec1) hist2@(Histogram sh2 vec2)
    | sh1 /= sh2 = error "Histograms are not of equal size."
    | otherwise  =
          (compareIntersect hist1 hist2 + V.sum (V.izipWith step vec1 vec2))
        * dist1Normalize
  where
    !(Z :. d :. h :. w) = sh1

    !maxZ = d - 1
    !maxH = h - 1
    !maxW = w - 1

    step ix v1 v2 =
        let !(Z :. z :. y :. x)    = fromLinearIndex ix
            !front | z == 0        = 
                   | otherwise   = ix - 1
            !left | x == 0     = maxIx
                  | otherwise   = ix - 1
            !rightIx | ix == maxIx = 0
                     | otherwise   = ix + 1
        in min (hist1 `index` ix1 leftIx + v1 + hist1 `index` ix1 rightIx)
               (hist2 `index` ix1 leftIx + v2 + hist2 `index` ix1 rightIx)
{-# SPECIALIZE compareHist1D :: Histogram DIM1 Float -> Histogram DIM1 Float
                             -> Float #-}

-- -----------------------------------------------------------------------------

directBinWeight :: Fractional a => a
directBinWeight = 1 - confCrossBinWeight

-- The weight of cross-bin comparisons as compared to a direct bin comparison
-- (1.0).

weightDist1, weightDist2, weightDist3 :: Fractional a => a
weightDist1 = confCrossBinWeight
weightDist2 = confCrossBinWeight / sqrt(2) -- sqrt(2) ~ 1.41
weightDist3 = confCrossBinWeight / sqrt(3) -- sqrt(3) ~ 1.73

-- The factor we need to apply when we are doing cross-bin comparisons.

dist1Normalize1, dist1Normalize2, dist1Normalize3 :: Fractional a => a
dist1Normalize = 1 / (1 + weightDist1)
dist2Normalize = 1 / (1 + weightDist1 + weightDist2)
dist3Normalize = 1 / (1 + weightDist1 + weightDist2 + weightDist3)
