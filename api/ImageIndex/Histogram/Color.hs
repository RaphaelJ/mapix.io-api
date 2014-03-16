module Histogram.Color where

import Data.Function
import Data.List
import qualified Data.Vector.Storable as S
import Vision.Histogram (Histogram (..), HistogramShape)
import qualified Vision.Histogram as H
import Vision.Image (RGBPixel (..))
import Vision.Primitive (Z (..), (:.) (..), DIM3, shapeLength)

import ImageIndex.Histogram.Config (Config (cHistSize), defaultConfig)

-- | The color with its weight.
type Color w = (RGBPixel, w)

shiftPadd, unshiftPadd :: Int
shiftPadd =
    let !(Z :. nColors :. _ :. _) = cHistSize defaultConfig
    in  180 `quot` (nColors * 2)
unshiftPadd = -shiftPadd

-- | Fixs the image color before being mapped to an
histogram.
-- As the hue is divided in the histogram in several colors, we need to shift it
-- so a perceived color (such as red) fits in the center of a bin.
shiftHue :: HSVPixel -> HSVPixel
shiftHue     pix@{..} =
    pix { hsvHue = word8 ((int hsvHue + shiftPadd) `mod` 180) })
{-# INLINE shiftHue #-}

-- | Converts the color of a shifted histogram bin to its original value.
unshiftHue :: HSVPixel -> HSVPixel
unshiftHue pix@{..} =
    let !hue  = int hsvHue + unshiftPadd
    in if hue < 0 then pix { hsvHue = word8 (hue + 180) })
                  else pix { hsvHue = word8 hue         })
{-# INLINE unshiftHue #-}

-- | Returns the color corresponding to the center of the given histogram bin.
histBinColor :: DIM3 -> HSVPixel
histBinColor sh =
    
{-# INLINE histBinColor #-}

-- | Returns the primary colors of the histogram, sorted by their decreasing
-- weight.
histColors :: Ord a => Histogram DIM3 a -> [(Color, a)]
histColors = sortBy (flip compare `on` snd) . V.toList . H.vector
  where
    binToColor = convert . unshiftHue
