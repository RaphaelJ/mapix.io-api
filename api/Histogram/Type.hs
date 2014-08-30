module Histogram.Type where

import Vision.Histogram (Histogram (..))
import Vision.Primitive (DIM1, DIM3)

type ColorIX = DIM3

type GreyIX = DIM1

data HeterogeneousHistogram a = HeterogeneousHistogram {
      hhColors :: !(Histogram ColorIX a)
    , hhGreys  :: !(Histogram GreyIX  a)
    }
