module Histogram.Compare (
      compareHeterogeneous, comparePartial
    ) where

import Prelude

import Data.Function
import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable)
import Vision.Histogram (compareIntersect, linearIndex)

import Histogram.Type (HeterogeneousHistogram (..), PartialHistogram (..))

compareHeterogeneous :: (Ord a, Num a, Storable a)
            => HeterogeneousHistogram a -> HeterogeneousHistogram a -> a
compareHeterogeneous hist hist' =
      (compareIntersect `on` hhColors) hist hist'
    + (compareIntersect `on` hhGreys)  hist hist'
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
