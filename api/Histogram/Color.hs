-- | Provides primitives to remaps colors from and to histogram bins.
module Histogram.Color (
      Color (..), ColorHistPixel (..), GreyHistPixel (..)
    , isGreyscale, toColorHistPixel, toGreyHistPixel
    , toColors, hsvToBin, binToHsv
    , hueToBin, satToColorBin, valToColorBin, valToGreyBin
    , binToHue, colorBinToSat, colorBinToVal, greyBinToVal
    , colorsHistSize, greysHistSize
    ) where

import Prelude

import Data.Function
import Data.List
import Data.Ratio
import Data.Word
import Foreign.Storable (Storable)
import Vision.Histogram (ToHistogram (..))
import Vision.Image (Pixel (..), RGBPixel (..), HSVPixel (..), convert)
import Vision.Primitive (Z (..), (:.) (..), ix1, ix3)

import qualified Data.Vector.Storable as VS
import qualified Vision.Histogram as H

import Histogram.Config (
      confHueBins, confHistColorMinSat, confHistColorMinVal
    , confHistNHues, confHistNSats, confHistNVals
    )
import Histogram.Type (Weight, ColorIX, GreyIX, HeterogeneousHistogram (..))

-- | The color with its weight.
data Color = Color {
      cColor  :: !RGBPixel
    , cWeight :: !Weight
    } deriving Show

-- Color and greyscale pixels --------------------------------------------------

-- Remarks: these instances remap pixels into histograms of the size given by
-- Histogram.Config.

-- | Pixels indexed into the color histogram.
--
-- As low value and saturation values are not mapped in the color histogram,
-- these values are re-mapped in this type.
newtype ColorHistPixel = ColorHistPixel { chpHsv :: HSVPixel }
    deriving (Show, Storable)

instance Pixel ColorHistPixel where
    type PixelChannel ColorHistPixel = PixelChannel HSVPixel

    pixNChannels = pixNChannels . chpHsv
    pixIndex     = pixIndex . chpHsv

instance ToHistogram ColorHistPixel where
    type PixelValueSpace ColorHistPixel = ColorIX

    pixToIndex !(ColorHistPixel (HSVPixel h s v)) =
        ix3 (hueToBin h) (satToColorBin s) (valToColorBin v)

    domainSize _ = colorsHistSize

-- | Pixels indexed into the grey-scale histogram.
--
-- Only the value of HSV pixels is indexed in the grey-scale histogram.
newtype GreyHistPixel = GreyHistPixel { ghpWord8 :: Word8 }
    deriving (Storable)

instance Pixel GreyHistPixel where
    type PixelChannel GreyHistPixel = Word8

    pixNChannels = pixNChannels . ghpWord8
    pixIndex     = pixIndex . ghpWord8

instance ToHistogram GreyHistPixel where
    type PixelValueSpace GreyHistPixel = GreyIX

    pixToIndex !(GreyHistPixel v) = ix1 $! valToGreyBin v

    domainSize _ = greysHistSize

-- | Returns 'True' if the color is to be mapped to the grey-scale part of the
-- 'HeterogeneousHistogram'.
isGreyscale :: HSVPixel -> Bool
isGreyscale (HSVPixel _ s v) =    int v < confHistColorMinVal
                               || int s < confHistColorMinSat

-- | Returns a 'ColorHistPixel' if the given 'HSVPixel' is mapped to the color
-- histogram.
toColorHistPixel :: HSVPixel -> Maybe ColorHistPixel
toColorHistPixel !pix
    | isGreyscale pix = Nothing
    | otherwise       = Just $! ColorHistPixel pix
{-# INLINE toColorHistPixel #-}

-- | Returns a 'GreyHistPixel' if the given 'HSVPixel' is mapped to the grey
-- histogram.
toGreyHistPixel :: HSVPixel -> Maybe GreyHistPixel
toGreyHistPixel !pix@(HSVPixel {..})
    | isGreyscale pix = Just $! GreyHistPixel hsvValue
    | otherwise       = Nothing
{-# INLINE toGreyHistPixel #-}

-- -----------------------------------------------------------------------------

-- | Returns the primary RGB colors of the histogram, sorted by their decreasing
-- weight. Ignores colors which weight less than the given value.
toColors :: HeterogeneousHistogram -> Weight -> [Color]
toColors HeterogeneousHistogram {..} !minVal =
    sortBy (flip compare `on` cWeight) colors
  where
    ixs =    [ (Left  ix, v) | (ix, v) <- H.assocs hhColors, v >= minVal ]
          ++ [ (Right ix, v) | (ix, v) <- H.assocs hhGreys,  v >= minVal ]

    colors = [ Color (convert $ binToHsv ix) v | (ix, v) <- ixs ]

-- | Returns the histogram bin corresponding to the given pixel.
hsvToBin :: HSVPixel -> Either ColorIX GreyIX
hsvToBin !pix@(HSVPixel {..})
    | isGreyscale pix =
        Right $! H.pixToBin greysHistSize  (GreyHistPixel hsvValue)
    | otherwise       =
        Left  $! H.pixToBin colorsHistSize (ColorHistPixel pix)

-- | Returns the pixel corresponding to the center of the given histogram bin.
binToHsv :: Either ColorIX GreyIX -> HSVPixel
binToHsv !(Left  (Z :. h :. s :. v)) = HSVPixel (binToHue h) (colorBinToSat s)
                                                (colorBinToVal v)
binToHsv !(Right (Z :. v))           = HSVPixel 0 0 (greyBinToVal v)

-- Colors to indexes precomputed mappings --------------------------------------

-- | Precomputed mapping from hue values ([0; 180[) to hue bin indexes of the
-- color histogram ([0; length colorHues[).
hueToBin :: Word8 -> Int
hueToBin =
    (vec VS.!) . int
  where
    !vec = VS.fromList $ go 0 ixs

    ixs = zip (VS.toList confHueBins) [0..] ++ [(179, 0)]

    go vecIx _ | vecIx >= 180   = []
    go vecIx ~((end, bin):ends) =
        let !vecIxNext = end + 1
        in replicate (vecIxNext - vecIx) bin ++ go vecIxNext ends

-- | Precomputed mapping saturation values ([confHistColorMinSat; 256[) to
-- saturation bin indexes of the color histogram ([0; confHistNSat[).
satToColorBin :: Word8 -> Int
satToColorBin = remapIxs (confHistColorMinSat, 256) (0, confHistNSats) . int

-- | Precomputed mapping from value values ([confHistColorMinVal; 256[) to bin
-- indexes of the color histogram ([0; confHistNVal[).
valToColorBin :: Word8 -> Int
valToColorBin = remapIxs (confHistColorMinVal, 256) (0, confHistNVals) . int

-- | Precomputed mapping from value values ([0; 256[) to bin indexes of the
-- greyscale histogram ([0; confHistNVal[).
valToGreyBin :: Word8 -> Int
valToGreyBin = remapIxs (0, 256) (0, confHistNVals) . int

-- Indexes to colors precomputed mappings --------------------------------------

-- | Precomputed mapping from hue bin indexes of the color histogram
-- ([0; length colorHues[) to hue values ([0; 180[).
binToHue :: Int -> Word8
binToHue =
    word8 . (vec VS.!)
  where
    !vec = VS.generate confHistNHues remap

    -- Returns the center value of the given bin.
    remap bin =
        let -- start is the index of the first cell of the bin.
            !start | bin == 0  = (VS.last confHueBins)        + 1
                   | otherwise = (confHueBins VS.! (bin - 1)) + 1

            -- end is the index of the first cell of the next bin.
            !end = (confHueBins VS.! bin) + 1

            !binLen | bin == 0  = end + (180 - start)
                    | otherwise = end - start
        in (start + round (binLen % 2)) `mod` 180

-- | Precomputed mapping from saturation bin indexes of the color histogram
-- ([0; confHistNSat[) to saturation values ([confHistColorMinSat; 256[).
colorBinToSat :: Int -> Word8
colorBinToSat = word8 . remapIxs (0, confHistNSats) (confHistColorMinSat, 256)

-- | Precomputed mapping from value bin indexes of the color histogram
-- ([0; confHistNVal[) to value values ([confHistColorMinVal; 256[).
colorBinToVal :: Int -> Word8
colorBinToVal = word8 . remapIxs (0, confHistNVals) (confHistColorMinVal, 256)

-- | Precomputed mapping from value bin indexes of the greyscale histogram
-- ([0; confHistNVal[) to value values ([0; 256[).
greyBinToVal :: Int -> Word8
greyBinToVal = word8 . remapIxs (0, confHistNVals) (0, 256)

-- -----------------------------------------------------------------------------

-- | Creates a function which maps indexes in the given range of values
-- ([srcFrom; srcTo[) to a new range of indexes ([dstFrom; dstTo[) using a
-- precomputed vector.
remapIxs :: (Int, Int) -> (Int, Int) -> (Int -> Int)
remapIxs (srcFrom, srcTo) (dstFrom, dstTo) =
    \val -> (vec VS.! (val - srcFrom)) + dstFrom
  where
    !nSrc  = srcTo - srcFrom
    !nDst  = dstTo - dstFrom
    !scale = nDst % nSrc

    !vec   = VS.generate nSrc remap

    remap ix = round $! ((ratio ix + 0.5) * scale) - 0.5
{-# NOINLINE remapIxs #-} -- GHC stack overflows when inlining this function.

-- Constants -------------------------------------------------------------------

colorsHistSize :: ColorIX
colorsHistSize = ix3 confHistNHues confHistNSats confHistNVals

greysHistSize :: GreyIX
greysHistSize = ix1 confHistNVals

-- Casting ---------------------------------------------------------------------

int :: Integral a => a -> Int
int = fromIntegral

ratio :: Integral a => a -> Ratio a
ratio = fromIntegral

word8 :: Integral a => a -> Word8
word8 = fromIntegral
