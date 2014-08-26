module Histogram.Color (
      Color (..), shiftHue, histColors, colorsHist, histBinColor
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

    pixToIndex !(GreyHistPixel v) = valToGreyBin v

    domainSize _ = ix1 confHistNVal

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
    | otherwise       = Just $! ColorHistPixel pix { FIX
              hsvSat   = word8 (  (int hsvSat - confHistColorMinSat)
                                * 256 % (255 - confHistColorMinSat))
            , hsvValue = word8 (  (int hsvValue - confHistColorMinValue)
                                * 256 `quot` (255 - confHistColorMinValue))
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
  where
    colors = []
           ++ 

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
                                      | Color rgb weight <- colors ]
    in normalize $ Histogram confHistSize vec
  where
    domain = H.domainSize (undefined :: RGBPixel)

    hsvs = [ (convert rgb, weight) | Color rgb weight <- colors ]

    toHistLinearIndex =   toLinearIndex confHistSize
                        . H.toBin confHistSize domain
                        . H.pixToIndex
                        . shiftHue
                        . convert
{-# SPECIALIZE colorsToHist :: [Color Float] -> HeterogeneousHistogram Float #-}

-- colorToBin :: HSVPixel -> Either ColorIX GreyIX
-- colorToBin !pix
--     | isGreyscale pix         = Right $! toBin (ix1 nVal) (ix1 256) (ix1 v)
--     | otherwise               =
--         let !shifted = shiftHSV pix
--         in Left $! toBin confHistSize hsvDomain (H.pixToIndex shifted)
-- {-# INLINE colorToBin #-}

-- | Returns the color corresponding to the center of the given histogram bin.
-- Assumes that the pixel has been shifted before the histogram computation
-- (with 'shiftHSV').
binToColor :: Either ColorIX GreyIX -> HSVPixel
binToColor !(Left  (Z :. h :. s :. v)) =
    HSVPixel (word8 $! binToHueMap V.! h) (word8 $! binToSatMap V.!   s)
             (word8 $! colorBinToValMap V.! v)
binToColor !(Right (Z :. v)) = HSVPixel 0 0 (word8 $ greyBinToValMap V.! v)
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

-- Precomputed conversion vectors ----------------------------------------------

colorHuesVec :: V.Vector Int
colorHuesVec = V.fromList colorHues

-- | Precomputed mapping of hue values ([0;179]) to hue bin indexes.
hueToBinMap :: V.Vector Int
hueToBinMap =
    V.fromList $ go 0 ixs
  where
    ixs = zip (sort colorHues) [0..] ++ [(179, 0)]

    go vecIx _ | vecIx >= 180  = []
    go vecIx ((end, bin):ends) =
        let !vecIx' = end + 1
        in replicate (vecIx' - vecIx) bin ++ go vecIx' ends

-- | Precomputed mapping from hue bins of the color histogram to hue values.
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

satToColorBinMap :: V.Vector Int
satToColorBinMap =
    V.generate (256 - minVal) binToVal
  where
    valToBin !val = -- val is in [0; 256 - minVal[.
        let !shifted = (val * 256) % (256 - minVal)
        in truncate $! shifted / binSize

    !binSize = nVals % nBins

    !middle = binSize / 2

buildMap nBinsFrom nBinsTo =
    

-- | Precomputed mapping from saturation bins indexes ([0; confHistNSat[) of the
-- color histogram to saturation values.
binToSatMap :: V.Vector Int
binToSatMap = binToValMap confHistNSat 256 confHistColorMinSat

-- | Precomputed mapping from value bins indexes ([0; confHistNVal[) of the
-- color histogram to value values.
colorBinToValMap :: V.Vector Int
colorBinToValMap = binToValMap confHistNVal 256 confHistColorMinValue

colorBinToVal :: Int -> Int

-- | Precomputed mapping from value values ([0; 256[) to bin indexes of the greyscale
-- histogram ([0; confHistNVal[).
valToGreyBin :: Int -> Int
valToGreyBin = remapIxs (0, 256) (0, confHistNVal)

-- | Precomputed mapping from value bin indexes of the greyscale histogram 
-- ([0; confHistNVal[) to value values ([0; 256[).
greyBinToVal :: Int -> Int
greyBinToVal = remapIxs (0, confHistNVal) (0, 256)

-- | Creates a function which maps indexes in the given range of values
-- ([srcFrom; srcTo[) to a new range of indexes ([dstFrom; dstTo[) using a
-- precomputed vector.
remapIxs :: (Int, Int) -> (Int, Int) -> (Int -> Int)
remapIxs (srcFrom, srcTo) (dstFrom, dstTo) =
    \!val -> (vec V.! (val - srcFrom)) + dstFrom
  where
    !nSrc  = srcTo - srcFrom
    !nDst  = dstTo - dstFrom
    !scale = nDst % nSrc

    !vec   = V.generate nSrc remap

    remap ix = round $! ((ratio val + 0.5) * scale) - 0.5
{-# INLINE remapIxs #-}

-- | Creates a vector which maps bins indexes in the given range of values
-- ([0; nBins[) to their original value ([minVal; nVals[).
binToValMap nBins nVals minVal =
    V.generate nBins binToVal
  where
    binToVal !bin = minVal + round (ratio bin * binSize + middle)

    !binSize = (nVals - minVal) % nBins
    !middle  = binSize / 2

-- Casting ---------------------------------------------------------------------

int :: Integral a => a -> Int
int = fromIntegral

ratio :: Integral a => a -> Ratio b
ratio = fromIntegral

word8 :: Integral a => a -> Word8
word8 = fromIntegral
