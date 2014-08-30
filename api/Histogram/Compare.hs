module Histogram.Compare (
      compareHist
    ) where

import Prelude

import Data.Function
import Foreign.Storable (Storable)
import Vision.Histogram (compareIntersect)

import Histogram.Type (HeterogeneousHistogram (..))

compareHist :: (Ord a, Num a, Storable a)
            => HeterogeneousHistogram a -> HeterogeneousHistogram a -> a
compareHist hist hist' =
      (compareIntersect `on` hhColors) hist hist'
    + (compareIntersect `on` hhGreys)  hist hist'
{-# SPECIALIZE compareHist :: HeterogeneousHistogram Float
                           -> HeterogeneousHistogram Float -> Float #-}
