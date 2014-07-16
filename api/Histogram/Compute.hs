-- | Computes the histogram from an image. Optionally removes the 
module Histogram.Compute (
      compute, histogramsAverage
    ) where

import Prelude

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Maybe
import Data.Ratio
import qualified Data.Vector.Storable as V
import Vision.Detector.Edge (canny)
import Vision.Histogram (Histogram (..))
import qualified Vision.Histogram as H
import Vision.Image (
      Image, GreyImage, GreyPixel, DelayedMask, MutableManifest, HSVDelayed
    , RGBImage, RGBAPixel
    , SeparableFilter, StorageImage (..)
    )
import qualified Vision.Image as I
import Vision.Primitive (Z (..), (:.) (..), ix2)

import Histogram.Color (shiftHue)
import Histogram.Config (cMaxImageSize, cHistSize, defaultConfig)

computeHist :: Bool -> Bool -> StorageImage -> Histogram
computeHist !ignoreBack !ignoreSkin !io =
    if null masks
       then histogram rgb
       else let !globMask  = foldl1' andMasks masks
                !maskedRgb =
                    I.fromFunction (I.shape rgb) $ \pt ->
                        if globMask `I.index` pt then Just $! rgb `I.index` pt
                                                 else Nothing
            in histogram maskedRgb
  where
    !maxSize = cMaxImageSize defaultConfig

    !(Z :. h :. w) = case io of GreyStorage img -> I.shape img
                                RGBAStorage img -> I.shape img
                                RGBStorage  img -> I.shape img

    -- Resized original image if larger than the maximum image size.
    !io' | h <= maxSize && w <= maxSize = io
         | otherwise                    =
            let !ratio   = max h w % maxSize
                !newSize = ix2 (round $ fromIntegral h * ratio)
                               (round $ fromIntegral w * ratio)
                resize = I.resize I.Bilinear newSize
            in case io of GreyStorage img -> resize img
                          RGBAStorage img -> resize img
                          RGBStorage  img -> resize img

    rgb :: RGBImage
    !rgb = I.convert io'

    masks = catMaybes [
          case io' of RGBAStorage rgba -> Just $! alphaMask rgba
                      _                -> Nothing
        , if ignoreBack then Just $! backgroundMask io'
                        else Nothing
        ]

    histogram rgb =
        let hsv = I.map (shiftHue . I.convert) rgb :: HSVDelayed
        in Histogram $! H.histogram (Just (cHistSize defaultConfig))

    -- Does an && between two masks boolean pixels.
    andMasks !m1 !m2 = I.fromFunction (I.shape m1) $ \pt ->
                            m1 `I.index` pt && m2 `I.index` pt

alphaMask = I.map (\pix -> I.rgbaAlpha pix == maxBound)

backgroundMask img =
    I.map (/= backgroundVal) flooded
  where
    blurRadius, sobelRadius, cannyLow, cannyHigh :: Int
    blurRadius  = 3
    sobelRadius = 2
    cannyLow    = 64
    cannyHigh   = 256

    backgroundVal = 127
    edgeVal       = maxBound

    grey, blurred, edges, closed :: GreyImage
    !grey    = I.convert img :: GreyImage
    !blurred = I.apply grey blur
    !edges   = canny blurred sobelRadius cannyLow cannyHigh
    !closed  = closure 2 edges

    closure rad img =
        let img' :: I.GreyImage
            img' = img `I.apply` I.dilate rad
        in img' `I.apply` I.erode rad

    !flooded = I.create $ do
        mut <- I.thaw closed :: ST s (MutableManifest GreyPixel s)
        fillIfNotEdge (ix2 0     0)     mut
        fillIfNotEdge (ix2 (h-1) 0)     mut
        fillIfNotEdge (ix2 (h-1) (w-1)) mut
        fillIfNotEdge (ix2 0     (w-1)) mut
        return mut

    -- Only fills from the corner if the corner is not an edge.
    fillIfNotEdge pt mut = do
        let val = I.read mut pt
        when (val /= backgroundVal) $
            I.floodFill pt mut backgroundVal

    blur :: SeparableFilter GreyPixel Float GreyPixel
    !blur = I.gaussianBlur blurRadius Nothing

    !(Z :. h :. w) = I.shape img

normalize = H.normalize 1.0

-- | Computes the average of a set of histograms. Returns a normalized histogram
-- (sum hist == 1.0).
histsAvg [hist] = normalize hist
histsAvg hists  =
    let hists' = map normalize hists
        n      = fromIntegral $ length hists
    in H.map (/ n) $ foldl1 addHists hists'
  where
    addHists !(Histogram sh1 vec1) !(Histogram sh2 vec2)
        | sh1 /= sh2 = error "Histograms are not of equal size."
        | otherwise  = let vecSum = V.zipWith (+) vec1 vec2
                       in Histogram sh1 vecSum
