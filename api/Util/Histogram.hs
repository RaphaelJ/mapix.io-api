{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util.Histogram (
      CompressedHistogram (..), compress, decompress, size
    ) where

import Prelude
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Vector.Storable as SV
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as UV
import Data.Word
import Database.Persist.Sql (PersistFieldSql (..))
import Foreign.Storable (Storable (..))
import Vision.Histogram (Histogram (..))
import Vision.Primitive (Shape, Z (..), (:.) (..), DIM5, shapeLength)
import Yesod

-- Histogram compression -------------------------------------------------------

-- | Compressed 'Histogram' using a run-length based encoding.
-- See <https://en.wikipedia.org/wiki/Run-length_encoding>.
data CompressedHistogram sh a = CompressedHistogram {
      shape  :: !sh
    , vector :: !(UV.Vector (Word8, a)) -- ^ Repetitions of the symbol 'a'.
    } deriving (Eq, Show)

compress :: (Storable a, Unbox a, Eq a)
         => Histogram sh a -> CompressedHistogram sh a
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
{-# SPECIALIZE compress
    :: Histogram sh Float -> CompressedHistogram sh Float #-}

decompress :: (Shape sh, Storable a, Unbox a, Eq a)
         => CompressedHistogram sh a -> Histogram sh a
decompress !(CompressedHistogram sh vec) =
    Histogram sh (SV.unfoldrN (shapeLength sh) step (0, vec))
  where
    step (i, vec') = let (!n, a) = UV.head vec'
                     in if n > i then Just (a, (i + 1, vec'))
                                 else step (0, UV.tail vec')
{-# INLINE decompress #-}

-- | Returns an estimation of the number of bytes needed to store the compressed
-- 'Histogram'.
size :: (Storable a, Unbox a) => CompressedHistogram sh a -> Int
size !(CompressedHistogram _ vec) =
    UV.length vec * (sizeOf (undefined :: Word8) + sizeOf (symbol vec))
  where
    symbol :: UV.Vector (Word8, a) -> a
    symbol _ = undefined

-- Histogram serialisation -----------------------------------------------------

instance S.Serialize (Histogram DIM5 Float) where
    put (Histogram (Z :. h :. s :. v :. y :. x) vec) = do
        S.put h >> S.put s >> S.put v >> S.put y >> S.put x
        SV.forM_ vec S.putFloat32le

    get = do
        h <- S.get
        s <- S.get
        v <- S.get
        y <- S.get
        x <- S.get

        let !sh  = Z :. h :. s :. v :. y :. x
            !len = shapeLength sh
        vec <- SV.replicateM len S.getFloat32le

        return $! Histogram sh vec

-- Histogram in database -------------------------------------------------------

instance PersistField (Histogram DIM5 Float) where
    toPersistValue = PersistByteString . S.encode

    fromPersistValue ~(PersistByteString bs) =
        either (Left . T.pack) Right $ S.decode bs

instance PersistFieldSql (Histogram DIM5 Float) where
    sqlType _ = SqlBlob

int :: Integral a => a -> Int
int = fromIntegral
word8 :: Integral a => a -> Word8
word8 = fromIntegral
