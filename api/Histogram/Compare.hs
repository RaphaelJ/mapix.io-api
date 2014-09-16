module Histogram.Compare (
      compareHeterogeneous, comparePartial
    ) where

import Prelude

import Data.Function
import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable)
import Vision.Histogram (Histogram (..), compareIntersect, index, linearIndex)
import Vision.Primitive (Z (..), (:.) (..), DIM1, ix1, shapeList)

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
compareHist1D hist1@(Histogram sh1 _) hist2@(Histogram sh2 _)
    | sh1 /= sh2 = error "Histograms are not of equal size."
    | otherwise  =
          compareIntersect hist1 hist2  * confPerfectFitBinWeight
        + sum [ min v1 v2 | Z :. ix <- shapeList sh1
              , let !v1 =   hist1 `index` ix1 (ix - 1)
                          + hist1 `index` ix1 ix
                          + hist1 `index` ix1 (ix + 1)
              , let !v2 =   hist2 `index` ix1 (ix - 1)
                          + hist2 `index` ix1 ix
                          + hist2 `index` ix1 (ix + 1)
              ]
          * inv / 3
  where
    !inv = 1.0 - confPerfectFitBinWeight
