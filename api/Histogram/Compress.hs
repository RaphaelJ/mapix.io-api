{-# LANGUAGE BangPatterns #-}
-- | Provides a run-length based compression algorithm to compress histograms.
module Histogram.Compress (
      CompressedHistogram (..), compress, decompress, size
    ) where

import Prelude
import Data.Word
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
    , vector :: !(VU.Vector (Word8, Weight)) -- ^ Repetitions of the symbol 'a'.
    } deriving (Eq, Show)

compress :: Histogram sh Weight -> CompressedHistogram sh
compress !(Histogram sh vec) =
    CompressedHistogram sh (VU.unfoldr step vec)
  where
    step vec' | VS.null vec' = Nothing
              | otherwise    =
                let !a = VS.head vec'
                    !n = 1 + (VS.length $ VS.takeWhile (== a)
                                        $ VS.take maxTail
                                        $ VS.tail vec')
                in Just ((word8 n, a), VS.drop n vec')

    !maxTail = int (maxBound - 1 :: Word8)

decompress :: Shape sh => CompressedHistogram sh -> Histogram sh Weight
decompress !(CompressedHistogram sh vec) =
    Histogram sh (VS.unfoldrN (shapeLength sh) step (0, vec))
  where
    step (i, vec') = let (!n, !a) = VU.head vec'
                     in if n > i then Just (a, (i + 1, vec'))
                                 else step (0, VU.tail vec')
{-# INLINE decompress #-}

-- | Returns an estimation of the number of bytes needed to store the compressed
-- 'Histogram'.
size :: CompressedHistogram sh -> Int
size !(CompressedHistogram _ vec) =
    VU.length vec * (sizeOf (undefined :: Word8) + sizeOf (symbol vec))
  where
    symbol :: VU.Vector (Word8, a) -> a
    symbol _ = undefined

int :: Integral a => a -> Int
int = fromIntegral

word8 :: Integral a => a -> Word8
word8 = fromIntegral
