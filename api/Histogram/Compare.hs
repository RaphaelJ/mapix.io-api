module Histogram.Compare (
      compareHeterogeneous, comparePartial
    ) where

import Prelude

import Data.Function
import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable)
import Vision.Histogram (Histogram (..), compareIntersect, index, linearIndex)
import Vision.Primitive (DIM1, ix1, shapeLength)

import Histogram.Config (confPerfectFitBinWeight)
import Histogram.Type (HeterogeneousHistogram (..), PartialHistogram (..))

compareHeterogeneous :: (Ord a, Num a, Storable a)
            => HeterogeneousHistogram a -> HeterogeneousHistogram a -> a
compareHeterogeneous hist1 hist2 =
      (compareIntersect `on` hhColors) hist1 hist2
    + (compareIntersect `on` hhGreys)  hist1 hist2
{-# SPECIALIZE compareHeterogeneous :: HeterogeneousHistogram Float
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

compareHist1D :: (Storable a, Ord a, Fractional a)
            => Histogram DIM1 a -> Histogram DIM1 a -> a
compareHist1D hist1@(Histogram sh1 vec1) hist2@(Histogram sh2 vec2)
    | sh1 /= sh2 = error "Histograms are not of equal size."
    | otherwise  =
          compareIntersect hist1 hist2      * confPerfectFitBinWeight
        + V.sum (V.izipWith step vec1 vec2) * inv / 3
  where
    !inv = 1.0 - confPerfectFitBinWeight

    !maxIx = shapeLength sh1 - 1

    step ix v1 v2 =
        let !leftIx  | ix == 0     = maxIx
                     | otherwise   = ix - 1
            !rightIx | ix == maxIx = 0
                     | otherwise   = ix + 1
        in min (hist1 `index` ix1 leftIx + v1 + hist1 `index` ix1 rightIx)
               (hist2 `index` ix1 leftIx + v2 + hist2 `index` ix1 rightIx)
{-# SPECIALIZE compareHist1D :: Histogram DIM1 Float -> Histogram DIM1 Float
                             -> Float #-}
