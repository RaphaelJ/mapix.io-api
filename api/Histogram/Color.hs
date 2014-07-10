module Histogram.Color (
      Color (..), shiftHue, histColors, colorsHist, histBinColor
    ) where

import Data.Char
import Data.Function
import Data.List
import qualified Data.Vector.Storable as V
import Data.Text (Text)
import Text.Printf
import Vision.Histogram (Histogram (..), HistogramShape,)
import qualified Vision.Histogram as H
import Vision.Image (RGBPixel (..), HSVPixel (..), convert)
import Vision.Primitive (Z (..), (:.) (..), DIM3, shapeLength, toLinearIndex)

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

-- | Constructs an hitsogram from the given list of weighted colors.
colorsHist :: Num a => DIM3 -> [Color a] -> Histogram DIM3 a
colorsHist size colors =
    let initial = V.replicate (shapeLength size) 0
        vec     = V.accum (+) initial [ (toHistLinearIndex rgb, v)
                                      | Color rgb v <- colors ]
    in H.normalize $ Histogram size vec
  where
    domain = H.domainSize (undefined :: RGBPixel)

    toHistLinearIndex =   toLinearIndex size
                        . H.toBin size domain
                        . H.pixToIndex
                        . shiftHue
                        . convert
{-# SPECIALIZE colorsHist :: [Color Float] -> Histogram DIM3 Float #-}

-- | Returns the color corresponding to the center of the given histogram bin.
-- Assumes that the hue has been shifted before the histogram computation (with
-- 'shiftHue'). 
histBinColor :: DIM3 -> HSVPixel
histBinColor !(Z :. h :. s :. v) =  HSVPixel h (s + middleSat) (v + middleVal)
{-# INLINE histBinColor #-}

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
