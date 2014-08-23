module Histogram.Color (
      Color (..), normalize, shiftHue, histColors, colorsHist, histBinColor
    ) where

import Prelude

import Data.Function
import Data.List
import Data.Ratio
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.Storable (Storable)
import Vision.Histogram (Histogram (..))
import qualified Vision.Histogram as H
import Vision.Image (RGBPixel (..), HSVPixel (..), convert)
import Vision.Primitive (Z (..), (:.) (..), DIM3, shapeLength, toLinearIndex)

import Histogram.Config (confHistSize)
import Histogram.Type (HeterogeneousHistogram (..))

-- | The color with its weight.
data Color w = Color {
      cColor  :: !RGBPixel
    , cWeight :: !w
    } deriving Show

newtype ColorHistPixel = ColorHistPixel HSVPixel
    deriving (Storable, Pixel)

-- | Defines how hue are mapped to the histogram. Each value defines the upper
-- inclusive value of the mapped value. The first bin starts at the end of the
-- last bin.
colorHues :: [Int]
colorHues = [7, 21, 32, 83, 97, 130, 143, 167]

instance ToHistogram ColorHistPixel where
    type PixelValueSpace ColorHistPixel = ColorIX

    pixToIndex !(ColorHistPixel (HSVPixel h s v)) =
        ix3 (hueToBinMap V.! h) s v

    domainSize _ = ix3 (V.length colorHuesVec) 256 256

newtype GreyHistPixel = GreyHistPixel Word8
    deriving (Storable, Pixel)

instance ToHistogram GreyHistPixel where
    type PixelValueSpace GreyHistPixel = GreyIX

    pixToIndex !(GreyHistPixel v) = ix1 v

    domainSize _ = ix1 256

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

-- | Returns a 'ColorHistPixel' if the given 'HSVPixel' is mapped to the color
-- histogram.
-- As low value and saturation values are not mapped in the color histogram,
-- these are re-mapped in the color histogram.
toColorHistPixel :: HSVPixel -> Maybe ColorHistPixel
toColorHistPixel pix@(HSVPixel {..})
    | isGreyscale pix = Nothing
    | otherwise       = Just $! ColorHistPixel pix {
              hsvSat   = word8 (  (int hsvSat - confHistColorMinSat)
                                * 255 `quot` (255 - confHistColorMinSat))
            , hsvValue = word8 (  (int hsvValue - confHistColorMinValue)
                                * 255 `quot` (255 - confHistColorMinValue))
            }
{-# INLINE toColorHistPixel #-}

-- | Returns a 'GreyHistPixel' if the given 'HSVPixel' is mapped to the grey
-- histogram.
toGreyHistPixel :: HSVPixel -> Maybe GreyHistPixel
toGreyHistPixel pix@(HSVPixel _ _ v) | isGreyscale pix = Nothing
                                     | otherwise       = Just $ GreyHistPixel v
{-# INLINE toGreyHistPixel #-}

-- | Returns the primary RGB colors of the histogram, sorted by their decreasing
-- weight. Ignores colors which weight less than the given value.
histToColors :: (Ord a, Storable a) => HeterogeneousHistogram a -> a
             -> [Color a]
histToColors !hist !minVal =
    sortBy (flip compare `on` cWeight) [ Color (convert $ histBinColor ix) v
                                       | (ix, v) <- H.assocs hist, v >= minVal ]
{-# SPECIALIZE histToColors :: HeterogeneousHistogram Float -> Float
                            -> [Color Float] #-}

-- | Constructs an histogram from the given list of weighted colors.
colorsToHist :: (Fractional a, Real a, Storable a)
           => [Color a] -> Histogram DIM3 a
colorsToHist colors =
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
{-# SPECIALIZE colorsToHist :: [Color Float] -> HeterogeneousHistogram Float #-}

colorToBin :: HSVPixel -> Either ColorIX GreyIX
colorToBin !pix
    | isGreyscale pix         = Right $! toBin (ix1 nVal) (ix1 255) (ix1 v)
    | otherwise               =
        let !shifted = shiftHSV pix
        in Left $! toBin confHistSize hsvDomain (H.pixToIndex shifted)
{-# INLINE colorToBin #-}

-- | Returns the color corresponding to the center of the given histogram bin.
-- Assumes that the pixel has been shifted before the histogram computation
-- (with 'shiftHSV').
binToColor :: Either ColorIX GreyIX -> HSVPixel
binToColor !(Left  ix) =
    let Z :. h :. s :. v = H.toBin hsvDomain confHistSize ix
    in HSVPixel (word8 $! binToHueMap V.! h) (word8 $! s + middleSat)
                (word8 $! v + middleVal)
binToColor !(Right ix) =
    let Z :. v = H.toBin (ix1 255) (ix1 nVal) ix
    in HSVPixel 0 0 (word8 $ v + middleVal)
{-# INLINE binToColor #-}

-- Constants -------------------------------------------------------------------

hsvDomain :: DIM3
hsvDomain = H.domainSize (undefined :: HSVPixel)

-- Number of each bins for each channel in the histogram.
nHue, nSat, nVal :: Int
Z :. nHue :. nSat :. nVal = confHistSize

-- Number of different values for each channel.
maxHue, maxSat, maxVal :: Int
Z :. maxHue :. maxSat :. maxVal = H.domainSize (undefined :: HSVPixel)

colorHuesVec :: V.Vector Int
colorHuesVec = V.fromList colorHues

-- | Precomputed mapping of hue values to bin indexes.
hueToBinMap :: V.Vector Int
hueToBinMap =
    V.fromList $ go 0 ixs
  where
    ixs = zip (sort colorHues) [0..] ++ [(179, 0)]

    go vecIx _ | vecIx >= 180  = []
    go vecIx ((end, bin):ends) =
        let !vecIx' = end + 1
        in replicate (vecIx' - vecIx) bin ++ go vecIx' ends

-- | Precomputed mapping of bin indexes to hue values.
binToHueMap :: V.Vector Int
binToHueMap =
    V.generate (V.length colorHuesVec) binToHue
  where
    binToHue bin =
        let -- start is the index of the last cell of the preceding bin.
            !start | bin == 0  = V.last colorHuesVec
                   | otherwise = colorHuesVec V.! (bin - 1)

            -- end is the index of the last cell of the current bin.
            !end    = colorHuesVec V.! bin

            !binLen | bin == 0  = (end + 180) - start
                    | otherwise = end - start
        in (start + 1 + round (binLen % 2)) `mod` 180

-- Used to compute the color value of the center of a bin.
middleHue, middleSat, middleVal :: Int
middleHue = maxHue `quot` (nHue * 2)
middleSat = maxSat `quot` (nSat * 2)
middleVal = maxVal `quot` (nVal * 2)

int :: Integral a => a -> Int
int = fromIntegral

word8 :: Integral a => a -> Word8
word8 = fromIntegral
