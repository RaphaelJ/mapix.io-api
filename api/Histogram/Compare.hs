module Histogram.Compare (
      compareHeterogeneous, comparePartial
    , compareCrossBinHist1D
    ) where

import Prelude

import Control.Lens
import Control.Monad.Trans.State.Strict
import Data.Function
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS
import Foreign.Storable (Storable)
import Vision.Histogram (Histogram (..), compareIntersect, index, linearIndex)
import Vision.Primitive (Z, (:.), DIM1, DIM3, ix1, shapeLength)

import Histogram.Config (confCrossBinWeight)
import Histogram.Type (HeterogeneousHistogram (..), PartialHistogram (..))

-- | Computes the intersection of two histograms.
--
-- Returns 'Nothing' if it can ensure that the intersection will be smaller than
-- the given minimum score.
compareHeterogeneous :: (Ord a, Num a, Storable a)
                     => a -> HeterogeneousHistogram a
                     -> HeterogeneousHistogram a -> Maybe a
compareHeterogeneous minScore hist1 hist2 = do
    let intersecOn         = compareIntersect . on
        !intersecGrey      = intersecOn hhGreys hist1 hist2 * directBinWeight
        !maxIntersecColors =    (min `on` hhColorsSum) hist1 hist2
                              * directBinWeight

    guard $! intersecGrey + maxIntersecColors + confCrossBinWeight >= 1.0

    let !intersecColors = compareOn hhColors hist1 hist2 * directBinWeight

    guard $! intersecGrey + intersecColors    + confCrossBinWeight >= 1.0

    return $!   intersecGrey + intersecColors
              + compareCrossBinHist1D + compareCrossBinHist3D
{-# SPECIALIZE compareHeterogeneous :: Float -> HeterogeneousHistogram Float
                                    -> HeterogeneousHistogram Float -> Float #-}

comparePartial :: (Storable a, Ord a, Fractional a)
               => PartialHistogram a -> HeterogeneousHistogram a -> a
comparePartial PartialHistogram {..} HeterogeneousHistogram {..} =
      (   intersec hhColors phColorsIxs phColorsValues
        + intersec hhGreys  phGreysIxs  phGreysValues)
    / phSumValues
  where
    intersec hist ixs vals =
        let step !ix !val = min val (hist `linearIndex` ix)
        in VS.sum $ VS.zipWith step ixs vals
{-# SPECIALIZE comparePartial :: PartialHistogram Float
                              -> HeterogeneousHistogram Float -> Float #-}

-- | Applies a pseudo cross-bin comparison of two histograms. Compare the sum
-- of each bin and its direct neighbors of the first histogram with the
-- corresponding sum in the second histogram.
--
-- @compareCrossBinHist1D intersec hist1 hist2@ where
-- @intersec == compareIntersect hist1 hist2@.
compareCrossBinHist1D :: (Storable a, Ord a, Fractional a)
                      => a -> Histogram DIM1 a -> Histogram DIM1 a -> a
compareCrossBinHist1D intersec hist1@(Histogram sh1 vec1)
                               hist2@(Histogram sh2 vec2)
    | sh1 /= sh2 = error "Histograms are not of equal size."
    | nBins <= 1 = intersec                                  -- [1]
    | otherwise  =
          (intersec + VS.sum (VS.izipWith step vec1 vec2) * crossBinFactor)
        * dist1Normalize
  where
    !nBins       = shapeLength sh1
    !maxIx       = nBins - 1

    step ix v1 v2 =
        let !hasLeft  = ix > 0
            !hasRight = ix < maxIx

        in if | hasLeft && hasRight      = f3 v1 v2 (ix - 1) (ix + 1)
              | hasLeft                  = f2 v1 v2 (ix - 1)
              | otherwise {- hasRight -} = f2 v1 v2 (ix + 1) -- Implied by [1].

    f2 !v1 !v2 !ix =
          min (hist1 `index` ix1 ix + v1) (hist2 `index` ix1 ix + v2)

    f3 !v1 !v2 !leftIx !rightIx =
          min (hist1 `index` ix1 leftIx + v1 + hist1 `index` ix1 rightIx)
              (hist2 `index` ix1 leftIx + v2 + hist2 `index` ix1 rightIx)

    -- As we compared more bins in the cross-bin comparison, we need to
    -- normalize the sum to the number of bin compared. Moreover the cross-bin
    -- comparison is weighted differently from the direct-bin comparison.
    !crossBinFactor =
        let !nF2         = 2
            !nF3         = realToFrac $! nBins - 2
            !nCrossBins  = nF2 * 2 + nF3 * 3
            !weightDist1 = confCrossBinWeight
        in nCrossBins / nBins * weightDist1
{-# SPECIALIZE compareCrossBinHist1D :: Float -> Histogram DIM1 Float
                                     -> Histogram DIM1 Float -> Float #-}

compareCrossBinHist3D :: (Storable a, Ord a, Fractional a)
                      => a -> Histogram DIM3 a -> Histogram DIM3 a -> a
compareCrossBinHist3D intersec hist1@(Histogram sh1 vec1)
                               hist2@(Histogram sh2 vec2)
    | sh1 /= sh2 = error "Histograms are not of equal size."
    | nBins <= 1 = intersec
    | otherwise  =
        let (!sum1D, !sum2D, !sum3D) = crossBinSums
            
        
          (intersec + VU.sum (VU.izipWith step vec1 vec2) * crossBinFactor)
        * dist1Normalize
  where
    !(Z :. nHues :. nSats :. nVals) = sh1

    crossBinSums = evalState (0, 0, 0) $ do
        let !maxHue = nHues - 1
            !maxSat = nSats - 1
            !maxVal = nVals - 1

            -- Precomputes left and right indexes for every saturation and value
            -- coordinates.
            !satsIxs = V.generate nSats (nonCyclicalIxs maxSat)
            !valsIxs = V.generate nVals (nonCyclicalIxs maxVal)

        VS.forM_ (VS.enumFromN 0 nHues) $ \h -> do
            let !hueIxs = cyclicalIxs h maxHue

            V.forM_ (VS.enumFromN 0 nSats) $ \s -> do
                let !satIxs = satsIxs V.! s

                V.forM_ (VS.enumFromN 0 nVals) $ \v -> do
                    let !valIxs  = valsIxs V.! v
                        !center1 = hist1 `index` ix3 h s v
                        !center2 = hist2 `index` ix3 h s v

                        sum1D !hist =
                               sumMapIx1 (\h' -> ix3 h' s v) hueIxs hist
                             + sumMapIx1 (\s' -> ix3 h s' v) satIxs hist
                             + sumMapIx1 (\v' -> ix3 h s v') valIxs hist
                        -- sum1D = length hueIxs (2) + length satIxs (0..2)
                        --       + length valIxs (0..2)

                        !sum1D1 = sum1D hist1 + center1
                        !sum1D2 = sum1D hist2 + center2
                        -- sum1D1/2 = sum1D + 1

                        sum2D !hist =
                               sumMapIx2 (\h' s' -> ix3 h' s' v) hueIxs satIxs
                                         hist
                             + sumMapIx2 (\h' v' -> ix3 h' s v') hueIxs valIxs
                                         hist
                             + sumMapIx2 (\s' v' -> ix3 h s' v') satIxs valIxs
                                         hist
                        -- sum2D = length hueIxs (2)    * length satIxs (0..2)
                        --       + length hueIxs (2)    * length valIxs (0..2)
                        --       + length satIxs (0..2) * length valIxs (0..2)

                        !sum2D1 = sum2D hist1 + sum1D1
                        !sum2D2 = sum2D hist2 + sum1D2
                        -- sum2D1/2 = sum2D + sum1D

                        sum3D !hist = sumMapIx3 hueIxs satIxs valIxs hist
                        -- sum3D = length hueIxs (2)    * length satIxs (0..2)
                        --       * length valIxs (0..2)

                        !sum3D1 = sum3D hist1 + sum2D1
                        !sum3D2 = sum3D hist2 + sum2D2

                    _1 += min sum1D1 sum1D2
                    _2 += min sum2D1 sum2D2
                    _3 += min sum3D1 sum3D2

    cyclicalIxs !maxIx !ix =
        VS.cons (                if ix == 0     then maxIx else ix - 1)
                (VS.singleton $! if ix == maxIx then 0     else ix + 1)

    nonCyclicalIxs !maxIx !ix =
        let !hasLeft  = ix > 0
            !hasRight = ix < maxIx
        in if | hasLeft   = if | hasRight  = VS.cons               (ix - 1)
                                                     (VS.singleton (ix + 1))
                               | otherwise = VS.singleton (ix - 1)
              | hasRight  = VS.singleton (ix + 1)
              | otherwise = VS.empty

    -- Sums the values at the indexes generated by the function iterated over
    -- a vector.
    -- > sumMapIx f vec1 hist = sum [ hist `index` f a | a <- vec ]
    sumMapIx1 !f !vec !hist = VS.foldl' (\acc a -> acc + hist `index` f a) 0 vec

    -- Sums the values at the indexes generated by the function iterated over
    -- two vectors.
    -- > sumMapIx2 f vec1 vec2 hist = sum [ hist `index` f a b
    --                                    | a <- vec1, b <- vec2 ]
    sumMapIx2 !f !vec1 !vec2 !hist =
        VS.foldl' (\acc a -> acc + sumMapIx1 (f a) vec2 hist) 0 vec1

    -- Sums the values at the indexes generated by the iteration over three 
    -- vectors.
    -- > sumMapIx3 vec1 vec2 vec3 hist = sum [ hist `index` ix3 a b c
    --                                       | a <- vec1, b <- vec2, c <- vec3 ]
    sumMapIx3 !f !vec1 !vec2 !vec3 !hist =
        VS.foldl' (\acc a -> acc + sumMapIx2 (ix3 a) vec2 vec3 hist)
                  0 vec1

    nBins1D =
        let !(!nSats1, !nSats2, !nSats3) | nSats == 1 = (1, 0, 0)
                                         | otherwise  = (0, 2, nSats - 2)

            !(!nVals1, !nVals2, !nVals3) | nVals == 1 = (1, 0, 0)
                                         | otherwise  = (0, 2, nVals - 2)

            nBins1D = nBins
                    + nBins * 2
                    + nSats2 + nSats3 * 2
                    + nVals2 + nVals3 * 2

            nBins2D =
                nHues * (   nSats1 * (nVals1 * 3 + nVals2 * 6 + nVals3 * 12)
                          + nSats2 * ()
                          + nSats3 * ())
        in
{-# SPECIALIZE compareHist3D :: Float -> Histogram DIM3 Float
                             -> Histogram DIM3 Float -> Float #-}

-- -----------------------------------------------------------------------------

directBinWeight :: Fractional a => a
directBinWeight = 1 - confCrossBinWeight

-- The weight of cross-bin comparisons as compared to a direct bin comparison
-- (1.0).

weightDist1, weightDist2, weightDist3 :: Fractional a => a
weightDist1 = confCrossBinWeight
weightDist2 = confCrossBinWeight / sqrt(2) -- sqrt(2) ~ 1.41
weightDist3 = confCrossBinWeight / sqrt(3) -- sqrt(3) ~ 1.73

-- The factor we need to apply when we are doing cross-bin comparisons.

dist1Normalize1, dist1Normalize2, dist1Normalize3 :: Fractional a => a
dist1Normalize = 1 / (1 + weightDist1)
dist2Normalize = 1 / (1 + weightDist1 + weightDist2)
dist3Normalize = 1 / (1 + weightDist1 + weightDist2 + weightDist3)
