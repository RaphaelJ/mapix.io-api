module Histogram.Compare (
      compareHist
    ) where

import Prelude

import Foreign.Storable (Storable)
import Vision.Histogram (Histogram, compareIntersect)
import Vision.Primitive (DIM3)

compareHist :: (Ord a, Num a, Storable a) 
            => Histogram DIM3 a -> Histogram DIM3 a -> a
compareHist hist1 hist2 = compareIntersect hist1 hist2
{-# SPECIALIZE compareHist :: Histogram DIM3 Float -> Histogram DIM3 Float
                           -> Float #-}
