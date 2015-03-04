module Histogram.Config where

import Prelude

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

import Histogram.Type (BinsSimilarity (..))

-- Hue bins --------------------------------------------------------------------

-- | Defines how hue are mapped to the histogram.
-- Upper inclusive value of the bin. Each bin starts after the end of the
-- previous bin. The first bin starts at the end of the last bin.
confHueBins :: VS.Vector Int
confHueBins = VS.fromList [
      4   -- 0. Red
    , 21  -- 1. Orange
    , 35  -- 2. Yellow
    , 70  -- 3. Green
    , 84  -- 4. Lime green/Spring green
    , 100 -- 5. Cyan
    , 129 -- 6. Blue
    , 140 -- 7. Purple
    , 168 -- 8. Pink
    ]

-- | Defines the similarity between every pair of bins in a cross bin
-- comparison.
confHueSimilarityMatrix :: V.Vector BinsSimilarity
confHueSimilarityMatrix = V.fromList [
      BinsSimilarity 0 1 0.5  -- Red & orange
    , BinsSimilarity 1 2 0.3  -- Orange & yellow
    , BinsSimilarity 2 3 0.2  -- Yellow & green
    , BinsSimilarity 3 4 0.5  -- Green & lime green
    , BinsSimilarity 4 5 0.5  -- Lime green & cyan
    , BinsSimilarity 5 6 0.4  -- Cyan & blue
    , BinsSimilarity 6 7 0.3  -- Blue & purple
    , BinsSimilarity 7 8 0.35 -- Purple & pink
    , BinsSimilarity 8 0 0.3  -- Pink & red
    ]

-- -----------------------------------------------------------------------------

-- | Minimum required saturation for an HSV color to be considered as having an
-- hue (pixels with a smaller saturation value will be considered as greyscale).
confHistColorMinSat :: Int
confHistColorMinSat = 20

-- | Minimum required value for an HSV color to be considered as not being black
-- (pixels with a smaller value will be considered as black).
confHistColorMinVal :: Int
confHistColorMinVal = 20

-- | Number of values bins in the color histogram.
confHistNHues :: Int
confHistNHues = VS.length confHueBins

-- | Number of saturation bins in the color histogram.
confHistNSats :: Int
confHistNSats = 4

-- | Number of value bins in the color and grey histograms.
confHistNVals :: Int
confHistNVals = 4

-- | Maximum size of a side of an image.
confMaxImageSize :: Int
confMaxImageSize = 350
