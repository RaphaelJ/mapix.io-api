module Histogram.Compare (
      compareHist
    ) where

import Prelude

import Foreign.Storable (Storable)
import Vision.Histogram (Histogram, compareIntersect)
import Vision.Primitive (DIM3)

import ImageIndex (IndexedHistogram)

compareHist :: (Ord a, Num a, Storable a)
            => Histogram DIM3 a -> Histogram DIM3 a -> a
compareHist hist1 hist2 = compareIntersect hist1 hist2
{-# SPECIALIZE compareHist :: IndexedHistogram -> IndexedHistogram -> Float #-}
