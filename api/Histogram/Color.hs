module Histogram.Color (
      ImageWithColors (..), Color (..)
    , normalize, shiftHue, histColors, colorsHist, histBinColor
    ) where

import Prelude

import Data.Function
import Data.List
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.Storable (Storable)
import Vision.Histogram (Histogram (..))
import qualified Vision.Histogram as H
import Vision.Image (RGBPixel (..), HSVPixel (..), convert)
import Vision.Primitive (Z (..), (:.) (..), DIM3, shapeLength, toLinearIndex)

import Histogram.Config (Config (cHistSize), defaultConfig)
import ImageIndex (Image)

-- | The JSON instance of this 'Image' wrapper will also display the main colors
-- of the image.
newtype ImageWithColors = ImageWithColors Image

-- | The color with its weight.
data Color w = Color {
      cColor  :: !RGBPixel
    , cWeight :: !w
    } deriving Show

-- | Normalize the histogram so the sum of its values equals 1.
normalize :: (Storable a, Real a, Fractional a)
          => Histogram sh a -> Histogram sh a
normalize = H.normalize 1.0
{-# SPECIALIZE normalize :: Histogram sh Float -> Histogram sh Float #-}

-- | Fixs the image color before being mapped to an histogram.
-- As the hue is divided in the histogram in several bins, we need to shift it
-- so a perceived color (such as red) fits in the center of a bin.
shiftHue :: HSVPixel -> HSVPixel
shiftHue pix@(HSVPixel {..}) = pix {
      hsvHue = word8 ((int hsvHue + middleHue) `mod` 180)
    }
{-# INLINE shiftHue #-}

-- | Returns the primary RGB colors of the histogram, sorted by their decreasing
-- weight. Ignores colors with a weight less than the given value.
histColors :: (Ord a, Storable a) => Histogram DIM3 a -> a -> [Color a]
histColors !hist !minVal =
    sortBy (flip compare `on` cWeight) [ Color (convert $ histBinColor ix) v
                                       | (ix, v) <- H.assocs hist, v >= minVal ]
{-# SPECIALIZE histColors :: Histogram DIM3 Float -> Float -> [Color Float] #-}

-- | Constructs an hitsogram from the given list of weighted colors.
colorsHist :: (Fractional a, Real a, Storable a)
           => DIM3 -> [Color a] -> Histogram DIM3 a
colorsHist size colors =
    let initial = V.replicate (shapeLength size) 0
        vec     = V.accum (+) initial [ (toHistLinearIndex rgb, val)
                                      | Color rgb val <- colors ]
    in normalize $ Histogram size vec
  where
    domain = H.domainSize (undefined :: RGBPixel)

    toHistLinearIndex =   toLinearIndex size
                        . H.toBin size domain
                        . H.pixToIndex
                        . shiftHue
                        . convert
{-# SPECIALIZE colorsHist :: DIM3 -> [Color Float] -> Histogram DIM3 Float #-}

-- | Returns the color corresponding to the center of the given histogram bin.
-- Assumes that the hue has been shifted before the histogram computation (with
-- 'shiftHue'). 
histBinColor :: DIM3 -> HSVPixel
histBinColor !(Z :. h :. s :. v) = HSVPixel (word8 h) (word8 $ s + middleSat) 
                                            (word8 $ v + middleVal)
{-# INLINE histBinColor #-}

-- Constants -------------------------------------------------------------------

-- Number of each bins for each channel in the histogram.
nHue, nSat, nVal :: Int
Z :. nHue :. nSat :. nVal = cHistSize defaultConfig

-- Number of different values for each channel.
maxHue, maxSat, maxVal :: Int
Z :. maxHue :. maxSat :. maxVal = H.domainSize (undefined :: HSVPixel)

-- Used to compute the color value of the center of a bin.
middleHue, middleSat, middleVal :: Int
middleHue = maxHue `quot` (nHue * 2)
middleSat = maxSat `quot` (nSat * 2)
middleVal = maxVal `quot` (nVal * 2)

int :: Integral a => a -> Int
int = fromIntegral

word8 :: Integral a => a -> Word8
word8 = fromIntegral
