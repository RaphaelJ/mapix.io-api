{-# LANGUAGE BangPatterns #-}

-- | Provides a run-length based compression algorithm to compress histograms.
module Histogram.Compress (
      CompressedHistogram (..), compress, decompress, size
    ) where

import ClassyPrelude

import Foreign.Storable (Storable (..))
import Vision.Histogram (Histogram (..))
import Vision.Primitive (Shape, shapeLength)

import qualified Data.Vector.Storable   as VS
import qualified Data.Vector.Unboxed    as VU

import Histogram.Type (Weight)

-- | Compressed 'Histogram' using a run-length based encoding.
-- See <https://en.wikipedia.org/wiki/Run-length_encoding>.
data CompressedHistogram sh = CompressedHistogram {
      shape  :: !sh
    , vector :: !(UVector (Word8, Weight)) -- ^ Repetitions of the symbol 'a'.
    } deriving (Eq, Show)

compress :: Histogram sh Weight -> CompressedHistogram sh
compress !(Histogram sh vec) =
    CompressedHistogram sh (VU.unfoldr step vec)
  where
    step vec' | null vec' = Nothing
              | otherwise =
                let !a = VS.head vec'
                    !n = 1 + (length $ takeWhile (== a)
                                     $ take maxTail
                                     $ drop 1 vec')
                in Just ((word8 n, a), drop n vec')

    !maxTail = int (maxBound - 1 :: Word8)

decompress :: Shape sh => CompressedHistogram sh -> Histogram sh Weight
decompress !(CompressedHistogram sh vec) =
    Histogram sh (VS.unfoldrN (shapeLength sh) step (0, vec))
  where
    step (i, vec') = let (!n, !a) = VU.head vec'
                     in if n > i then Just (a, (i + 1, vec'))
                                 else step (0, drop 1 vec')
{-# INLINE decompress #-}

-- | Returns an estimation of the number of bytes needed to store the compressed
-- 'Histogram'.
size :: CompressedHistogram sh -> Int
size !(CompressedHistogram _ vec) =
    length vec * (sizeOfWord8 + sizeOf (symbol vec))
  where
    symbol :: UVector (Word8, a) -> a
    symbol _ = error "Unreachable"

    sizeOfWord8 = sizeOf (error "Unreachable" :: Word8)

int :: Integral a => a -> Int
int = fromIntegral

word8 :: Integral a => a -> Word8
word8 = fromIntegral
