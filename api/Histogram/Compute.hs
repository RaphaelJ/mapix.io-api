-- | Functions to create an histogram from an image or a list of colors.
module Histogram.Compute (
      Mask
    , fromImages, fromImage, fromColors
    , resize
    , average, normalize, alphaMask, backgroundMask
    ) where

import Prelude

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Parallel.Strategies
import Data.Either
import Data.Function
import Data.List
import Data.Maybe
import Data.Ratio
import Foreign.Storable (Storable)
import Vision.Detector.Edge (canny)
import Vision.Histogram (Histogram (..))
import Vision.Image (
      Image, ImagePixel, ImageChannel, FromFunction, FromFunctionPixel
    , Interpolable
    , DelayedMask, Manifest, MutableManifest
    , Grey, GreyPixel, HSV, HSVPixel, RGBA, RGB, StorageImage (..)
    , SeparableFilter
    )
import Vision.Primitive (
      Z (..), (:.) (..), Point, Size, ix2, shapeLength, toLinearIndex
    )

import qualified Data.Vector.Storable   as V
import qualified Vision.Histogram       as H
import qualified Vision.Image           as I

import Histogram.Color (
      Color (..), ColorHistPixel, GreyHistPixel
    , toColorHistPixel, toGreyHistPixel, hsvToBin, colorsHistSize, greysHistSize
    )
import Histogram.Config (confMaxImageSize)
import Histogram.Type (HeterogeneousHistogram (..))

type Mask = Manifest Bool

-- | Builds an histogram from a list of images.
--
-- Returns a normalized histogram.
fromImages :: Bool -> Bool -> [StorageImage] -> HeterogeneousHistogram
fromImages !ignoreBack !ignoreSkin =   average
                                     . map (fromImage ignoreBack ignoreSkin)

-- | Builds an histogram from an image.
fromImage :: Bool -> Bool -> StorageImage -> HeterogeneousHistogram
fromImage !ignoreBack !ignoreSkin !io =
    let !io' = resize io

        hsv :: HSV
        !hsv = I.convert (I.convert io' :: RGB)

        masks = catMaybes [
              case io' of RGBAStorage rgba -> Just $! alphaMask rgba
                          _                -> Nothing
            , if ignoreBack then Just $! backgroundMask io'
                            else Nothing
            ]
    in if null masks
       then toHist hsv
       else let globMask :: Mask
                !globMask = foldl1' andMasks masks

                maskedHsv :: DelayedMask HSVPixel
                maskedHsv =
                    I.fromFunction (I.shape hsv) $ \pt ->
                        if globMask `I.index` pt then Just $! hsv `I.index` pt
                                                 else Nothing
            in toHist maskedHsv
  where

    toHist img =
        let colors :: DelayedMask ColorHistPixel
            colors = I.fromFunction (I.shape img) $   (>>= toColorHistPixel)
                                                    . (img `I.maskedIndex`)

            greys :: DelayedMask GreyHistPixel
            greys  = I.fromFunction (I.shape img) $   (>>= toGreyHistPixel)
                                                    . (img `I.maskedIndex`)

            colorsHist = H.histogram (Just colorsHistSize) colors
            greysHist  = H.histogram (Just greysHistSize)  greys

        in runEval $ HeterogeneousHistogram <$> rpar colorsHist
                                            <*> rseq greysHist
    {-# INLINE toHist #-}

    -- Does an && between two masks boolean pixels.
    andMasks !m1 !m2 = I.fromFunction (I.shape m1) $ \pt ->
                            m1 `I.index` pt && m2 `I.index` pt

-- | Constructs an histogram from the given list of weighted colors.
-- Returns a normalized histogram.
fromColors :: [Color] -> HeterogeneousHistogram
fromColors xs =
    let (colors, greys) = partitionEithers $ map colorToBin xs

    in normalize $ HeterogeneousHistogram (toHist colorsHistSize colors)
                                          (toHist greysHistSize  greys)
  where
    colorToBin Color {..} =
        case hsvToBin (I.convert cColor) of
            Left ix  -> Left  (ix, cWeight)
            Right ix -> Right (ix, cWeight)

    toHist histSize ixs =
        let initial = V.replicate (shapeLength histSize) 0
            vec     = V.accum (+) initial [ (toHistLinearIndex ix, weight)
                                          | (ix, weight) <- ixs ]

            toHistLinearIndex = toLinearIndex histSize
        in Histogram histSize vec

-- -----------------------------------------------------------------------------

-- | Resizes the original image if larger than the maximum image size.
resize :: StorageImage -> StorageImage
resize io | h <= confMaxImageSize && w <= confMaxImageSize = io
          | otherwise                                      =
                let !ratio   = max h w % confMaxImageSize
                    !newSize = ix2 (round $ fromIntegral h / ratio)
                                   (round $ fromIntegral w / ratio)
                in case io of
                        GreyStorage img -> GreyStorage $! resize' newSize img
                        RGBAStorage img -> RGBAStorage $! resize' newSize img
                        RGBStorage  img -> RGBStorage  $! resize' newSize img
  where
    !(Z :. h :. w) = case io of GreyStorage img -> I.shape img
                                RGBAStorage img -> I.shape img
                                RGBStorage  img -> I.shape img

    resize' :: (Interpolable (ImagePixel i), Image i, FromFunction i
              , FromFunctionPixel i ~ ImagePixel i
              , Integral (ImageChannel i))
            => Size -> i -> i
    resize' size img = I.resize I.NearestNeighbor size img
    {-# INLINE resize' #-}

-- -----------------------------------------------------------------------------

-- | Computes the average of a set of histograms.
-- Returns a normalized histogram.
average :: [HeterogeneousHistogram] -> HeterogeneousHistogram
average []     = error "Empty histogram list."
average [hist] = normalize hist
average hists  =
    let hists'  = map normalize hists
        n       = fromIntegral $ length hists
        sumHist = foldl1 addHeterogeneousHists hists'
    in HeterogeneousHistogram (H.map (/ n) $ hhColors sumHist)
                              (H.map (/ n) $ hhGreys  sumHist)
  where
    addHeterogeneousHists hist hist' =
        HeterogeneousHistogram ((addHists `on` hhColors) hist hist')
                               ((addHists `on` hhGreys)  hist hist')

    addHists !(Histogram sh vec) !(Histogram sh' vec')
        | sh /= sh' = error "Histograms are not of equal size."
        | otherwise = let vecSum = V.zipWith (+) vec vec'
                      in Histogram sh vecSum
{-# SPECIALIZE average :: [HeterogeneousHistogram Float]
                       -> HeterogeneousHistogram Float #-}

-- | Normalizes the 'HeterogeneousHistogram' so the sum of its values equals 1.
normalize :: HeterogeneousHistogram -> HeterogeneousHistogram
normalize HeterogeneousHistogram {..} =
    let !sumColors = histSum hhColors
        !sumGreys  = histSum hhGreys
        !total     = sumColors + sumGreys
        !colors | sumColors > 0 = H.normalize (sumColors / total) hhColors
                | otherwise     = hhColors
        !greys  | sumGreys  > 0 = H.normalize (sumGreys / total)  hhGreys
                | otherwise     = hhGreys
    in HeterogeneousHistogram colors greys
  where
    histSum = V.sum . H.vector
{-# SPECIALIZE normalize :: HeterogeneousHistogram Float
                         -> HeterogeneousHistogram Float #-}

-- -----------------------------------------------------------------------------

alphaMask :: RGBA -> Mask
alphaMask = I.map (\pix -> I.rgbaAlpha pix == maxBound)

-- | Detects the background region by flood-filling from the four sides up to
-- any edge detected by the Canny's algorithm.
backgroundMask :: StorageImage -> Mask
backgroundMask img =
    I.map (/= backgroundVal) flooded
  where
    !blurRadius  = 3
    !sobelRadius = 2
    !cannyLow    = 64
    !cannyHigh   = 256

    !backgroundVal = 127 -- Differentiates background pixels from edge (255) and
                         -- non-edge (0) pixels.

    grey, blurred, edges, closed :: Grey
    !grey    = I.convert img
    !blurred = blur `I.apply` grey
    !edges   = canny sobelRadius cannyLow cannyHigh blurred
    !closed  = closing 2 edges

    -- Applies a morphological closing of the given radius.
    closing rad img' =
        let img'' :: Grey
            !img'' = I.dilate rad `I.apply` img'
        in I.erode rad `I.apply` img''

    !flooded = I.create $ do
        mut <- I.thaw closed :: ST s (MutableManifest GreyPixel s)
        fillIfNotEdge (ix2 0     0)     mut
        fillIfNotEdge (ix2 (h-1) 0)     mut
        fillIfNotEdge (ix2 (h-1) (w-1)) mut
        fillIfNotEdge (ix2 0     (w-1)) mut
        return mut

    -- Only fills from the point if the point is not on an edge.
    fillIfNotEdge :: Point -> MutableManifest GreyPixel s -> ST s ()
    fillIfNotEdge pt mut = do
        val <- I.read mut pt
        when (not $ isEdge val) $
            I.floodFill pt backgroundVal mut

    isEdge = (== maxBound)

    blur :: SeparableFilter GreyPixel Float GreyPixel
    !blur = I.gaussianBlur blurRadius Nothing

    !(Z :. h :. w) = I.shape grey
