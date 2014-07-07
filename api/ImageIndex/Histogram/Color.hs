module ImageIndex.Histogram.Color (
      Color (..), shiftHue, histColors, histBinColor, rgb2Hex
    ) where

import Data.Char
import Data.Function
import Data.List
import qualified Data.Vector.Storable as S
import Data.Text (Text)
import Text.Printf
import Vision.Histogram (Histogram (..), HistogramShape)
import qualified Vision.Histogram as H
import Vision.Image (RGBPixel (..), HSVPixel (..))
import Vision.Primitive (Z (..), (:.) (..), DIM3, shapeLength)

import ImageIndex.Histogram.Config (Config (cHistSize), defaultConfig)

-- | The color with its weight.
data Color w = Color {
      cColor  :: !RGBPixel
    , cWeight :: !w
    } deriving Show

-- | Fixs the image color before being mapped to an histogram.
-- As the hue is divided in the histogram in several bins, we need to shift it
-- so a perceived color (such as red) fits in the center of a bin.
shiftHue :: HSVPixel -> HSVPixel
shiftHue pix@{..} = pix {
      hsvHue = word8 ((int hsvHue + middleHue) `mod` 180)
    }
{-# INLINE shiftHue #-}

-- | Returns the primary RGB colors of the histogram, sorted by their decreasing
-- weight. Ignores colors with a weight less than the given value.
histColors :: Ord a => Histogram DIM3 a -> a -> [Color a]
histColors !hist !minVal =
    sortBy (flip compare `on` snd) [ Color (histBinColor ix) v
                                   | (ix, v) <- H.assocs hist, v >= minVal ]
{-# SPECIALIZE histColors :: Histogram DIM3 Float -> Float -> [Color Float] #-}

-- | Returns the color corresponding to the center of the given histogram bin.
-- Assumes that the hue has been shifted before the histogram computation (with
-- 'shiftHue'). 
histBinColor :: DIM3 -> HSVPixel
histBinColor !(Z :. h :. s :. v) =  HSVPixel h (s + middleSat) (v + middleVal)
{-# INLINE histBinColor #-}

rgb2Hex :: RGBPixel -> Text
rgb2Hex !(RGBPixel r g b) =
    T.pack $! printf "#%s%s%s" (toHex r) (toHex g) (toHex b)
  where
    toHex v = let (q, r) = v `quotRem` 16 in [ intToDigit q, intToDigit r ]

-- Constants -------------------------------------------------------------------

-- Number of each bins for each channel in the histogram.
nHue, nSat, nVal :: Int
Z : nHue :. nSat :. nVal = cHistSize defaultConfig

-- Number of different values for each channel.
maxHue, maxSat, maxVal :: Int
Z : maxHue :. maxSat :. maxVal = H.domainSize (undefined :: HSVPixel)

-- Used to compute the color value of the center of a bin.
middleHue, middleSat, middleVal :: Int
middleHue = maxHue `quot` (nHue * 2)
middleSat = maxSat `quot` (nSat * 2)
middleVal = maxVal `quot` (nVal * 2)
