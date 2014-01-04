{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.Field where

import Prelude
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Vector.Storable as V
import Database.Persist.Sql (PersistFieldSql (..))
import Vision.Histogram (Histogram (..))
import Vision.Primitive (Z (..), (:.) (..), DIM5, shapeLength)
import Yesod

instance S.Serialize (Histogram DIM5 Float) where
    put (Histogram (Z :. h :. s :. v :. y :. x) vec) = do
        S.put h >> S.put s >> S.put v >> S.put y >> S.put x
        V.forM_ vec S.put

    get = do
        h <- S.get
        s <- S.get
        v <- S.get
        y <- S.get
        x <- S.get

        let !sh  = Z :. h :. s :. v :. y :. x
            !len = shapeLength sh
        vec <- V.replicateM len S.get

        return $! Histogram sh vec

instance PersistField (Histogram DIM5 Float) where
    toPersistValue = PersistByteString . S.encode

    fromPersistValue ~(PersistByteString bs) =
        either (Left . T.pack) Right $ S.decode bs

instance PersistFieldSql (Histogram DIM5 Float) where
    sqlType _ = SqlBlob
