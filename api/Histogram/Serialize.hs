{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Provides instances for the serialization and persistence of histograms.
module Histogram.Serialize () where

import Prelude

import Control.Applicative
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Vector.Storable as SV
import Database.Persist.Sql (PersistFieldSql (..))
import Vision.Histogram (Histogram (..))
import Vision.Primitive (Shape, Z (..), (:.) (..), shapeLength)
import Yesod

import Histogram.Type (HeterogeneousHistogram (..), ColorIX, GreyIX)

-- Shapes ----------------------------------------------------------------------

instance Serialize Z where
    put Z = return ()
    {-# INLINE put #-}

    get = return Z
    {-# INLINE get #-}

instance Serialize sh => Serialize (sh :. Int) where
    put (sh :. n) = S.put sh >> S.put n
    {-# INLINE put #-}

    get = (:.) <$> S.get <*> S.get
    {-# INLINE get #-}

-- Histograms ------------------------------------------------------------------

instance (Shape sh, Serialize sh) => Serialize (Histogram sh Float) where
    put (Histogram sh vec) = do
        S.put sh
        SV.forM_ vec S.putFloat32le

    get = do
        sh <- S.get
        Histogram sh <$> SV.replicateM (shapeLength sh) S.getFloat32le

instance Serialize (Histogram sh a) => PersistField (Histogram sh a) where
    toPersistValue = PersistByteString . S.encode

    fromPersistValue ~(PersistByteString bs) =
        either (Left . T.pack) Right $ S.decode bs

instance Serialize (Histogram sh a) => PersistFieldSql (Histogram sh a) where
    sqlType _ = SqlBlob

-- HeterogeneousHistogram ------------------------------------------------------

instance ( Serialize (Histogram ColorIX Weight)
         , Serialize (Histogram GreyIX Weight))
        => Serialize HeterogeneousHistogram where
    put HeterogeneousHistogram {..} = S.put hhColors >> S.put hhGreys

    get = HeterogeneousHistogram <$> S.get <*> S.get

instance Serialize HeterogeneousHistogram
        => PersistField HeterogeneousHistogram where
    toPersistValue = PersistByteString . S.encode

    fromPersistValue ~(PersistByteString bs) =
        either (Left . T.pack) Right $ S.decode bs

instance Serialize HeterogeneousHistogram
        => PersistFieldSql HeterogeneousHistogram where
    sqlType _ = SqlBlob
