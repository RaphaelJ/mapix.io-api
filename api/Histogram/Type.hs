module Histogram.Type () where

type ColorIX = DIM3

type GreyIX = DIM1

data HeterogeneousHistogram a = HeterogeneousHistogram {
      hhColors :: !(Histogram ColorIX a)
    , hhGreys  :: !(Histogram GreyIX  a)
    }
