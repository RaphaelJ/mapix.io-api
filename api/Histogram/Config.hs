module Histogram.Config (confHistSize, confMaxImageSize) where

import Prelude

import Vision.Primitive (DIM3, ix3)

confHistSize :: DIM3
confHistSize = ix3 8 4 4

-- | Minimum required saturation for an HSV color to be considered as having an
-- hue (pixels with a smaller saturation value will be considered as greyscale).
confHistColorMinSat :: Int
confHistColorMinSat = 20

-- | Minimum required value for an HSV color to be considered as not being black
-- (pixels with a smaller value will be considered as black).
confHistColorMinValue :: Int
confHistColorMinValue = 20

-- | Maximum size of a side of an image.
confMaxImageSize :: Int
confMaxImageSize = 480
