module Histogram.Type () where

data HeterogeneousHistogram a = HeterogeneousHistogram {
      hhColors :: Histogram DIM3 a
    , hhGreys  :: Histogram DIM1 a
    }
