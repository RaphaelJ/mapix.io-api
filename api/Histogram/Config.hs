module Histogram.Config (confHistSize, confMaxImageSize) where

import Prelude

import Vision.Primitive (DIM3, ix3)

confHistSize :: DIM3
confHistSize     = ix3 8 4 4

-- | Maximum size of a side of an image.
confMaxImageSize :: Int
confMaxImageSize = 480
