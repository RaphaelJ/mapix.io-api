{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Util.Hmac.Type where

import Prelude

import Control.Arrow (first)
import Data.String (IsString)
import Data.Text (Text)

import Database.Persist.Sql (PersistFieldSql)
import Yesod

-- | HMACs are unique identifiers used for files, uploads, comments and archive
-- files.
newtype Hmac = Hmac Text
    deriving ( Eq, Ord, IsString, PersistField, PersistFieldSql, PathPiece
             , ToJSON)

instance Show Hmac where
    show (Hmac txt) = show txt

instance Read Hmac where
    readsPrec n str = map (first Hmac) (readsPrec n str)
    readList str = map (first (map Hmac)) (readList str)
