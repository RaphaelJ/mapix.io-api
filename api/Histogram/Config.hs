module Histogram.Config (
      confHistColorMinSat, confHistColorMinValue, confHistNSat, confHistNVal
    , confMaxImageSize
    ) where

import Prelude

-- | Minimum required saturation for an HSV color to be considered as having an
-- hue (pixels with a smaller saturation value will be considered as greyscale).
confHistColorMinSat :: Int
confHistColorMinSat = 20

-- | Minimum required value for an HSV color to be considered as not being black
-- (pixels with a smaller value will be considered as black).
confHistColorMinValue :: Int
confHistColorMinValue = 20

-- | Number of saturation bins in the color histogram.
confHistNVal :: Int
confHistNSat = 4

-- | Number of value bins in the color and grey histograms.
confHistNVal :: Int
confHistNVal = 4

-- | Maximum size of a side of an image.
confMaxImageSize :: Int
confMaxImageSize = 480
