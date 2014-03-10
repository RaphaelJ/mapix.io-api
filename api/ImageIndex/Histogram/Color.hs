-- | Provides functions to extract the primary colors of an histogram.
module Histogram.Color where

import Data.Function
import Data.List
import qualified Data.Vector.Storable as S
import Vision.Histogram (Histogram (..))
import qualified Vision.Histogram as H
import Vision.Image (RGBPixel (..))
import Vision.Primitive (Z (..), (:.) (..), DIM3, shapeLength)

-- | The color with its weight.
type Color w = (RGBPixel, w)

-- | Returns the primary colors of the histogram, sorted by their decreasing
-- weight.
histColors :: Ord a => Histogram DIM3 a -> [(Color, a)]
histColors = sortBy (flip compare `on` snd) . V.toList . H.vector
