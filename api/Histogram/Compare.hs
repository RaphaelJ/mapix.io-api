module Histogram.Compare (
      compareHist
    ) where

compareHist :: Histogram DIM3 a -> Histogram DIM3 a -> a
compareHist hist1 hist2 = compareIntersect hist1 hist2
{-# SPECIALIZE compareHist :: Histogram DIM3 Float -> Histogram DIM3 Float
                           -> Float #-}
