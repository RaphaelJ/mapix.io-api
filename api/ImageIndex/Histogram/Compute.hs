-- | 
module ImageIndex.Histogram.Compute (
      preprocessImage, alphaMask, backgroundMask
    )

import Control.Monad.ST
import Data.List

import ImageIndex.Config (cMaxImageSize, defaultConfig)

import Vision.Image (
      GreyImage, GreyPixel, MutableManifest, RGBAPixel, SeparableFilter
    , StorageImage (..)
    )
import qualified Vision.Image as I
import Vision.Primitive (ix2)

preprocessImage :: Bool -> Bool -> StorageImage ->
preprocessImage !ignoreBackground !ignoreSkin !io =

  where
    -- Resizes the image if larger than the maximum image size.
    !resized
        | h <= maxSize && w <= maxSize = io
        | otherwise                    =
            let !ratio   = max h w % maxSize
                !newSize = ix2 (round $ fromIntegral h * ratio)
                               (round $ fromIntegral w * ratio)
                resize !img = I.resize img Bilinear newSize
            in case io of GreyStorage grey -> resize
                          RGBAStorage rgba -> grey
                          RGBStorage  rgb  -> rgb

    !(Z :. h :. w) = case io of GreyStorage grey -> I.shape grey
                                RGBAStorage rgba -> I.shape rgba
                                RGBStorage  rgb  -> I.shape rgb

    !maxSize = cMaxImageSize defaultConfig

alphaMask = I.map (\!(RGBAPixel _ _ _ a) -> a /= maxBound)

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
    !edges   = I.canny blurred sobelRadius cannyLow cannyHigh
    !closed  = closure 2 edges

    !flooded = I.create $ do
        mut <- I.thaw closed :: ST s (MutableManifest GreyPixel s)
        fillIfNotEdge (ix2 0     0)     mut
        fillIfNotEdge (ix2 (h-1) 0)     mut
        fillIfNotEdge (ix2 (h-1) (w-1)) mut
        fillIfNotEdge (ix2 0     (w-1)) mut
        return mut

    -- Only fills from the corner if the corner is not an edge.
    fillIfNotEdge pt mut = do
        val = I.read mut pt
        when (val /= backgroundVal) $
            I.floodFill pt mut backgroundVal

    blur :: SeparableFilter GreyPixel Float GreyPixel
    !blur = I.gaussianBlur blurRadius Nothing

    !(Z :. h :. w) = I.shape img

histogramAverage hists =
    let hists' = map (flip H.normalize 1.0) hists
    in normalize' $ foldl1 addHists hists'
  where
    normalize' = flip H.normalize 1.0

    addHists !(Histogram sh1 vec1) !(Histogram sh2 vec2) 
        | sh1 /= sh2 = error "Histograms are not of equal size."
        | otherwise  = let vecSum = V.zipWith (+) vec1 vec2
                       in Histogram sh1 vecSum
