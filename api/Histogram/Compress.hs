-- | Provides a run-length based compression algorithm to compress histograms.
module Histogram.Compress (
      CompressedHistogram (..), compress, decompress, size
    ) where

import Prelude
import Data.Word
import Data.Vector.Unboxed (Unbox)
import Foreign.Storable (Storable (..))
import Vision.Histogram (Histogram (..))
import Vision.Primitive (Shape, shapeLength)

import qualified Data.Vector.Storable   as SV
import qualified Data.Vector.Unboxed    as UV

-- | Compressed 'Histogram' using a run-length based encoding.
-- See <https://en.wikipedia.org/wiki/Run-length_encoding>.
data CompressedHistogram sh = CompressedHistogram {
      shape  :: !sh
    , vector :: !(UV.Vector (Word8, Weight)) -- ^ Repetitions of the symbol 'a'.
    } deriving (Eq, Show)

compress :: Histogram sh -> CompressedHistogram sh
compress !(Histogram sh vec) =
    CompressedHistogram sh (UV.unfoldr step vec)
  where
    step vec' | SV.null vec' = Nothing
              | otherwise    =
                let !a = SV.head vec'
                    !n = 1 + (SV.length $ SV.takeWhile (== a)
                                        $ SV.take maxTail
                                        $ SV.tail vec')
                in Just ((word8 n, a), SV.drop n vec')

    !maxTail = int (maxBound - 1 :: Word8)

decompress :: CompressedHistogram sh -> Histogram sh
decompress !(CompressedHistogram sh vec) =
    Histogram sh (SV.unfoldrN (shapeLength sh) step (0, vec))
  where
    step (i, vec') = let (!n, !a) = UV.head vec'
                     in if n > i then Just (a, (i + 1, vec'))
                                 else step (0, UV.tail vec')
{-# INLINE decompress #-}

-- | Returns an estimation of the number of bytes needed to store the compressed
-- 'Histogram'.
size :: CompressedHistogram sh -> Int
size !(CompressedHistogram _ vec) =
    UV.length vec * (sizeOf (undefined :: Word8) + sizeOf (symbol vec))
  where
    symbol :: UV.Vector (Word8, a) -> a
    symbol _ = undefined

int :: Integral a => a -> Int
int = fromIntegral

word8 :: Integral a => a -> Word8
word8 = fromIntegral
