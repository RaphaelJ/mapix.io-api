-- | Functions to create an histogram from an image or a list of colors.
module Histogram.Compute (
      fromImage, fromColors
    , average, normalize, alphaMask, backgroundMask
    ) where

import Prelude

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Maybe
import Data.Ratio
import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable)
import Vision.Detector.Edge (canny)
import Vision.Histogram (Histogram (..))
import qualified Vision.Histogram as H
import Vision.Image (
      Grey, GreyPixel, HSV, HSVPixel, RGBA, RGB, RGBPixel
    , DelayedMask, Manifest, MutableManifest, SeparableFilter, StorageImage (..)
    )
import qualified Vision.Image as I
import Vision.Primitive (Z (..), (:.) (..), DIM2, ix2)

import Histogram.Config (confHistSize, confMaxImageSize)
import Histogram.Type (HeterogeneousHistogram (..))

type Mask = Manifest Bool

-- fromImage :: Bool -> Bool -> StorageImage -> HeterogeneousHistogram a
fromImage !ignoreBack !ignoreSkin !io =
    if null masks
       then let !hsv = toHSV rgb :: HSV -- :: HSVDelayed
            in calcHist hsv
       else let globMask :: Mask
                !globMask = foldl1' andMasks masks :: Mask

                maskedRgb :: DelayedMask RGBPixel
                maskedRgb =
                    I.fromFunction (I.shape rgb) $ \pt ->
                        if globMask `I.index` pt then Just $! rgb `I.index` pt
                                                 else Nothing

                maskedHSV :: DelayedMask HSVPixel
                maskedHSV = toHSV maskedRgb

                colors = I.fromFunction (I.shape rgb) $ \pt ->
                        
                ap (toColorHistPixel

            in calcHist maskedHSV
  where
    !(Z :. h :. w) = case io of GreyStorage img -> I.shape img
                                RGBAStorage img -> I.shape img
                                RGBStorage  img -> I.shape img

    -- Resizes the original image if larger than the maximum image size.
    !io' | h <= confMaxImageSize && w <= confMaxImageSize = io
         | otherwise                                      =
            let !ratio   = max h w % confMaxImageSize
                !newSize = ix2 (round $ fromIntegral h / ratio)
                               (round $ fromIntegral w / ratio)
            in case io of GreyStorage img -> GreyStorage $! resize' newSize img
                          RGBAStorage img -> RGBAStorage $! resize' newSize img
                          RGBStorage  img -> RGBStorage  $! resize' newSize img

    resize' :: (I.Interpolable (I.ImagePixel i), I.Image i, I.FromFunction i
              , I.FromFunctionPixel i ~ I.ImagePixel i
              , Integral (I.ImageChannel i))
            => DIM2 -> i -> i
    resize' size img = I.resize I.Bilinear size img
    {-# INLINE resize' #-}

    rgb :: RGB
    !rgb = I.convert io'

    masks = catMaybes [
          case io' of RGBAStorage rgba -> Just $! alphaMask rgba
                      _                -> Nothing
        , if ignoreBack then Just $! backgroundMask io'
                        else Nothing
        ]

    toHSV = I.convert

    calcHist = H.histogram Nothing

    -- Does an && between two masks boolean pixels.
--     andMasks :: Mask -> Mask -> Mask
    andMasks !m1 !m2 = I.fromFunction (I.shape m1) $ \pt ->
                            m1 `I.index` pt && m2 `I.index` pt

-- | Constructs an histogram from the given list of weighted colors.
fromColors :: (Fractional a, Real a, Storable a)
           => [Color a] -> HeterogeneousHistogram a
fromColors colors =
    let (greys, colors) = partitionEithers $ map colorToBin colors

    in normalize (toHist colorsHistSize colors) (toHist initialGreys greys)
  where
    colorToBin Color {..} =
        let !hsv = convert cColor
        in case pixToBin hsv of
                Left ix  -> Left  (ix, cWeight)
                Right ix -> Right (ix, cWeight)

    toHist histSize ixs =
        let initial = V.replicate (shapeLength histSize) 0
            vec     = V.accum (+) initial [ (toHistLinearIndex ix, weight)
                                          | (ix, weight) <- ixs ]

            toHistLinearIndex = toLinearIndex histSize
        in Histogram histSize vec
{-# SPECIALIZE fromColors :: [Color Float] -> HeterogeneousHistogram Float #-}

-- -----------------------------------------------------------------------------

-- | Computes the average of a set of histograms. Returns a normalized histogram
-- (sum hist == 1.0).
average :: (Storable a, Real a, Fractional a, Eq sh)
         => HeterogeneousHistogram a -> HeterogeneousHistogram a
average [hist] = normalize hist
average hists  =
    let hists' = map normalize hists
        n      = fromIntegral $ length hists
    in H.map (/ n) $ foldl1 addHists hists'
  where
    addHists !(Histogram sh1 vec1) !(Histogram sh2 vec2)
        | sh1 /= sh2 = error "Histograms are not of equal size."
        | otherwise  = let vecSum = V.zipWith (+) vec1 vec2
                       in Histogram sh1 vecSum
{-# SPECIALIZE average :: HeterogeneousHistogram Float
                       -> HeterogeneousHistogram Float #-}

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

-- -----------------------------------------------------------------------------

alphaMask :: RGBAImage -> Mask
alphaMask = I.map (\pix -> I.rgbaAlpha pix == maxBound)

backgroundMask :: StorageImage -> Mask
backgroundMask img =
    I.map (/= backgroundVal) flooded
  where
    blurRadius  = 3
    sobelRadius = 2
    cannyLow    = 64
    cannyHigh   = 256

    backgroundVal = 127
    edgeVal       = maxBound

    grey, blurred, edges, closed :: Grey
    !grey    = I.convert img
    !blurred = I.apply grey blur
    !edges   = canny sobelRadius cannyLow cannyHigh blurred
    !closed  = closing 2 edges

    -- Applies a morphological closing of the given radius.
    closing rad img' =
        let img'' :: Grey
            img'' = img' `I.apply` I.dilate rad
        in img'' `I.apply` I.erode rad

    !flooded = I.create $ do
        mut <- I.thaw closed :: ST s (MutableManifest GreyPixel s)
        fillIfNotEdge (ix2 0     0)     mut
        fillIfNotEdge (ix2 (h-1) 0)     mut
        fillIfNotEdge (ix2 (h-1) (w-1)) mut
        fillIfNotEdge (ix2 0     (w-1)) mut
        return mut

    -- Only fills from the point if the point is not on an edge.
    fillIfNotEdge pt mut = do
        val <- I.read mut pt
        when (val /= edgeVal) $
            I.floodFill pt backgroundVal mut

    blur :: SeparableFilter GreyPixel Float GreyPixel
    !blur = I.gaussianBlur blurRadius Nothing

    !(Z :. h :. w) = I.shape grey
