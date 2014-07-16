{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Provides instances for the serialization and persistence of 'Float'
-- histograms.
module Histogram.Serialize () where

import Prelude
import Control.Applicative
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Vector.Storable as SV
import Database.Persist.Sql (PersistFieldSql (..))
import Vision.Histogram (Histogram (..))
import Vision.Primitive (Shape, Z (..), (:.) (..), shapeLength)
import Yesod

instance S.Serialize Z where
    put Z = return ()
    {-# INLINE put #-}

    get = return Z
    {-# INLINE get #-}

instance S.Serialize sh => S.Serialize (sh :. Int) where
    put (sh :. n) = S.put sh >> S.put n
    {-# INLINE put #-}

    get = (:.) <$> S.get <*> S.get
    {-# INLINE get #-}

instance (Shape sh, S.Serialize sh) => S.Serialize (Histogram sh Float) where
    put (Histogram sh vec) = do
        S.put sh
        SV.forM_ vec S.putFloat32le

    get = do
        sh <- S.get
        Histogram sh <$> SV.replicateM (shapeLength sh) S.getFloat32le

instance (Shape sh, S.Serialize sh) => PersistField (Histogram sh Float) where
    toPersistValue = PersistByteString . S.encode

    fromPersistValue ~(PersistByteString bs) =
        either (Left . T.pack) Right $ S.decode bs

instance (Shape sh, S.Serialize sh)
    => PersistFieldSql (Histogram sh Float) where
    sqlType _ = SqlBlob
