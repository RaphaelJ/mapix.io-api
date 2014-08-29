module Histogram.Color (
      Color (..), ColorHistPixel (..), GreyHistPixel (..)
          
    , isGreyscale, toColorHistPixel toGreyHistPixel
    , histToColors, binToColor
      
      shiftHue, histColors, colorsHist, histBinColor, remapIxs
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

-- Color and greyscale pixels --------------------------------------------------

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
        ix3 (hueToBin h) (satToColorBin s) (valToColorBin v)

    domainSize _ = colorsHistSize

newtype GreyHistPixel = GreyHistPixel Word8
    deriving (Storable, Pixel)

instance ToHistogram GreyHistPixel where
    type PixelValueSpace GreyHistPixel = GreyIX

    pixToIndex !(GreyHistPixel v) = valToGreyBin v

    domainSize _ = greysHistSize

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
    | otherwise       = Just $! ColorHistPixel pix
{-# INLINE toColorHistPixel #-}

-- | Returns a 'GreyHistPixel' if the given 'HSVPixel' is mapped to the grey
-- histogram.
toGreyHistPixel :: HSVPixel -> Maybe GreyHistPixel
toGreyHistPixel pix@(HSVPixel _ _ v) | isGreyscale pix = Nothing
                                     | otherwise       = Just $ GreyHistPixel v
{-# INLINE toGreyHistPixel #-}

-- -----------------------------------------------------------------------------

-- | Returns the primary RGB colors of the histogram, sorted by their decreasing
-- weight. Ignores colors which weight less than the given value.
histToColors :: (Ord a, Storable a) => HeterogeneousHistogram a -> a
             -> [Color a]
histToColors HeterogeneousHistogram {..} !minVal =
    sortBy (flip compare `on` cWeight) colors
  where
    ixs =    [ (Left  ix, v) | (ix, v) <- H.assocs hhGreys,  v >= minVal ]
          ++ [ (Right ix, v) | (ix, v) <- H.assocs hhColors, v >= minVal ]

    colors = [ Color (convert $ binToColor ix) v | (ix, v) <- ixs ]
{-# SPECIALIZE histToColors :: HeterogeneousHistogram Float -> Float
                            -> [Color Float] #-}

-- -- | Returns the histogram bin corresponding to the given color.
-- colorToBin :: HSVPixel -> Either ColorIX GreyIX
-- colorToBin !pix
--     | isGreyscale pix         = Right $! toBin (ix1 nVal) (ix1 256) (ix1 v)
--     | otherwise               =
--         let !shifted = shiftHSV pix
--         in Left $! toBin confHistSize hsvDomain (H.pixToIndex shifted)

-- | Returns the color corresponding to the center of the given histogram bin.
binToColor :: Either ColorIX GreyIX -> HSVPixel
binToColor !(Left  (Z :. h :. s :. v)) = HSVPixel (binToHue h) (colorBinToSat s)
                                                  (colorBinToVal v)
binToColor !(Right (Z :. v))           = HSVPixel 0 0 (greyBinToVal v)

-- Constants -------------------------------------------------------------------

colorsHistSize :: ColorIX
colorsHistSize = ix3 (V.length colorHuesVec) confHistNSat confHistNVal

greysHistSize :: GreyIX
greysHistSize = ix1 confHistNVal

-- hsvDomain :: DIM3
-- hsvDomain = H.domainSize (undefined :: HSVPixel)
-- 
-- -- Number of different values for each channel.
-- maxHue, maxSat, maxVal :: Int
-- Z :. maxHue :. maxSat :. maxVal = H.domainSize (undefined :: HSVPixel)

-- Colors to indexes precomputed mappings --------------------------------------

-- | Precomputed mapping from hue values ([0; 180[) to hue bin indexes of the
-- color histogram ([0; length colorHues[).
hueToBin :: Word8 -> Int
hueToBin =
    (vec V.!) . int
  where
    !vec = V.fromList $ go 0 ixs

    ixs = zip (sort colorHues) [0..] ++ [(179, 0)]

    go vecIx _ | vecIx >= 180  = []
    go vecIx ((end, bin):ends) =
        let !vecIx' = end + 1
        in replicate (vecIx' - vecIx) bin ++ go vecIx' ends

-- | Precomputed mapping saturation values ([confHistColorMinSat; 256[) to
-- saturation bin indexes of the color histogram ([0; confHistNSat[).
satToColorBin :: Word8 -> Int
satToColorBin = remapIxs (confHistColorMinSat, 256) (0, confHistNSat) . int

-- | Precomputed mapping from value values ([confHistColorMinVal; 256[) to bin
-- indexes of the color histogram ([0; confHistNVal[).
valToColorBin :: Word8 -> Int
valToColorBin = remapIxs (confHistColorMinVal, 256) (0, confHistNVal) . int

-- | Precomputed mapping from value values ([0; 256[) to bin indexes of the
-- greyscale histogram ([0; confHistNVal[).
valToGreyBin :: Word8 -> Int
valToGreyBin = remapIxs (0, 256) (0, confHistNVal) . int

-- Indexes to colors precomputed mappings --------------------------------------

-- | Precomputed mapping from hue bin indexes of the color histogram
-- ([0; length colorHues[) to hue values ([0; 180[).
binToHue :: Int -> Word8
binToHue =
    word8 . (vec V.!)
  where
    !vec = V.generate (V.length colorHuesVec) binToHue

    binToHue bin =
        let -- start is the index of the first cell of the bin.
            !start | bin == 0  = V.last colorHuesVec        + 1
                   | otherwise = colorHuesVec V.! (bin - 1) + 1

            -- end is the index of the first cell of the next bin.
            !end    = colorHuesVec V.! bin + 1

            !binLen | bin == 0  = (end + 180) - start
                    | otherwise = end - start
        in (start + round (binLen % 2)) `mod` 180

-- | Precomputed mapping from saturation bin indexes of the color histogram
-- ([0; confHistNSat[) to saturation values ([confHistColorMinSat; 256[).
colorBinToSat :: Int -> Word8
colorBinToSat = word8 . remapIxs (0, confHistNSat) (confHistColorMinSat, 256)

-- | Precomputed mapping from value bin indexes of the color histogram
-- ([0; confHistNVal[) to value values ([confHistColorMinVal; 256[).
colorBinToVal :: Int -> Word8
colorBinToVal = word8 . remapIxs (0, confHistNVal) (confHistColorMinVal, 256)

-- | Precomputed mapping from value bin indexes of the greyscale histogram
-- ([0; confHistNVal[) to value values ([0; 256[).
greyBinToVal :: Int -> Word8
greyBinToVal = word8 . remapIxs (0, confHistNVal) (0, 256)

-- -----------------------------------------------------------------------------

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

-- Casting ---------------------------------------------------------------------

int :: Integral a => a -> Int
int = fromIntegral

ratio :: Integral a => a -> Ratio b
ratio = fromIntegral

word8 :: Integral a => a -> Word8
word8 = fromIntegral
