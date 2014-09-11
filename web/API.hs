{-# LANGUAGE OverloadedStrings #-}

module API (
      ApiImageCode (..), Tag, HasAPIConfig
    , postImage
    ) where

import Control.Monad
import Data.Conduit.Binary (sourceLbs)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql (..))
import Network.HTTP.Conduit (def, parseUrl)
import Network.HTTP.Types.Method (methodPost)

newtype ApiImageCode = ApiImageCode { aicValue :: Text }
    deriving (Eq, Ord, Read, PersistField)

instance PersistFieldSql ApiImageCode where
    sqlType action = sqlType (aicValue `liftM` action)

type Tag = Text

class HasAPIConfig a where
    getApiRoot :: a -> Text
    getApiKey  :: a -> Text

postImage :: (MonadReader env m, HasAPIConfig env)
          => FilePath -> [Tag] -> m ApiImageCode
postImage path tags =
    req <- getBaseReq

    let req' = req {
              method      = methodPost
            , path        = "/images"
            , requestBody = requestBodySourceChunked $ sourceFile path
            }
    in withResponse req' $ \resp ->
            
  where
    sourceFile path 

-- -----------------------------------------------------------------------------

getBaseReq = (parseUrl . getApiRoot) <$> ask

lbsToRequest = requestBodySourceChunked . sourceLbs
