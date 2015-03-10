{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Provides instances for the types from 'ObjectIndex.Type'.
module ObjectIndex.Instance () where

import Prelude

import Control.Arrow
import Control.Monad
import Database.Persist.Sql (PersistFieldSql (..))
import Text.Parsec (parse)
import Text.Printf
import Yesod

import qualified Data.Text as T

import ObjectIndex.Type
import ObjectIndex.Tag (tagPathParser, tagPath2Text)

instance Show ObjectCode where
    show (ObjectCode txt) = show txt

instance Read ObjectCode where
    readsPrec n str = map (first ObjectCode) (readsPrec n str)

instance PersistFieldSql ObjectCode where
    sqlType action = sqlType (ocValue `liftM` action)

instance Show TagPath where
    show = T.unpack . tagPath2Text

instance PathPiece TagPath where
    fromPathPiece txt =
        case parse tagPathParser "" txt of Right path -> Just path
                                           Left _     -> Nothing

    toPathPiece = tagPath2Text

instance Show TagExpression where
    show (TagName name)       = show name
    show (TagNot expr)        = printf "!(%s)" (show expr)
    show (TagAnd expr1 expr2) = printf "(%s) && (%s)" (show expr1) (show expr2)
    show (TagOr  expr1 expr2) = printf "(%s) || (%s)" (show expr1) (show expr2)
