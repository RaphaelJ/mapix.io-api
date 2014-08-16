module Histogram.Color (
      Color (..), normalize, shiftHue, histColors, colorsHist, histBinColor
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

import Histogram.Config (confHistSize)
import ImageIndex (IndexedHistogram)

-- | The color with its weight.
data Color w = Color {
      cColor  :: !RGBPixel
    , cWeight :: !w
    } deriving Show

newtype ShiftedHSV = ShiftedHSV HSVPixel
    deriving (Storable, Pixel)

-- | Normalize the 'HeterogeneousHistogram' so the sum of its values equals 1.
normalize :: (Storable a, Real a, Fractional a)
          => HeterogeneousHistogram a -> HeterogeneousHistogram a
normalize HeterogeneousHistogram {..} =
    let !sumColors = histSum hhColors
        !sumGreys  = histSum hhGreys
        !total     = sumColors + sumGreys

        normalize' s = H.normalize (s / total)
    in HeterogeneousHistogram (normalize' sumColors hhColors)
                              (normalize' sumGreys  hhGreys)
  where
    histSum = V.sum . H.vector
{-# SPECIALIZE normalize :: HeterogeneousHistogram Float
                         -> HeterogeneousHistogram Float #-}

-- | Returns 'True' if the color is to be mapped to the greyscale part of the
-- 'HeterogeneousHistogram'.
isGreyscale :: HSVPixel -> Bool
isGreyscale (HSVPixel _ s v) =    v < confHistColorMinValue
                               || s < confHistColorMinSat

-- | Fixs the image color before being mapped to the color histogram.
-- As the hue is divided in the histogram in several bins, we need to shift it
-- so a perceived color (such as red) fits in the center of a bin.
shiftHSV :: HSVPixel -> ShiftedHSV
shiftHSV pix@(HSVPixel {..}) = pix {
      hsvHue   = word8 ((int hsvHue + middleHue) `mod` 180)
    , hsvSat   = word8 (  (int hsvSat - confHistColorMinSat)
                        * 255 `mod` (255 - confHistColorMinSat))
    , hsvValue = word8 (  (int hsvValue - confHistColorMinValue)
                        * 255 `mod` (255 - confHistColorMinValue))
    }
{-# INLINE shiftHue #-}

-- | Returns the primary RGB colors of the histogram, sorted by their decreasing
-- weight. Ignores colors which weight less than the given value.
histColors :: (Ord a, Storable a) => Histogram DIM3 a -> a -> [Color a]
histColors !hist !minVal =
    sortBy (flip compare `on` cWeight) [ Color (convert $ histBinColor ix) v
                                       | (ix, v) <- H.assocs hist, v >= minVal ]
{-# SPECIALIZE histColors :: IndexedHistogram -> Float -> [Color Float] #-}

-- | Constructs an histogram from the given list of weighted colors.
colorsHist :: (Fractional a, Real a, Storable a)
           => [Color a] -> Histogram DIM3 a
colorsHist colors =
    let initial = V.replicate (shapeLength confHistSize) 0
        vec     = V.accum (+) initial [ (toHistLinearIndex rgb, val)
                                      | Color rgb val <- colors ]
    in normalize $ Histogram confHistSize vec
  where
    domain = H.domainSize (undefined :: RGBPixel)

    toHistLinearIndex =   toLinearIndex confHistSize
                        . H.toBin confHistSize domain
                        . H.pixToIndex
                        . shiftHue
                        . convert
{-# SPECIALIZE colorsHist :: [Color Float] -> IndexedHistogram #-}

-- | Returns the color corresponding to the center of the given histogram bin.
-- Assumes that the hue has been shifted before the histogram computation (with
-- 'shiftHue').
histBinColor :: DIM3 -> HSVPixel
histBinColor !ix =
    let domain = H.domainSize (undefined :: HSVPixel)
        Z :. h :. s :. v = H.toBin domain confHistSize ix
    in HSVPixel (word8 h) (word8 $ s + middleSat) (word8 $ v + middleVal)
{-# INLINE histBinColor #-}

-- Constants -------------------------------------------------------------------

-- Number of each bins for each channel in the histogram.
nHue, nSat, nVal :: Int
Z :. nHue :. nSat :. nVal = confHistSize

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
