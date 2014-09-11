{-# LANGUAGE OverloadedStrings #-}

module API (
      ApiImageCode (..), Tag, HasAPIConfig (..)
    , postImage
    ) where

import Prelude

import Control.Lens
import Control.Monad
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (String), encode)
import Data.Aeson.Lens (key)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql (..))
import Network.HTTP.Client.Conduit (HasHttpManager (getHttpManager))
import Network.Wreq (
      Options
    , defaults, header, manager, partLBS, partFileSource, postWith, responseBody
    )

newtype ApiImageCode = ApiImageCode { aicValue :: Text }
    deriving (Eq, Ord, Read, PersistField)

instance PersistFieldSql ApiImageCode where
    sqlType action = sqlType (aicValue `liftM` action)

type Tag = Text

class HasAPIConfig a where
    getApiRoot :: a -> String
    getApiKey  :: a -> ByteString

postImage :: ( MonadReader env m, HasHttpManager env, HasAPIConfig env
             , MonadIO m)
          => FilePath -> [Tag] -> m (Maybe ApiImageCode)
postImage path tags = do
    options <- getOptions
    url     <- getResourceUrl "/images"

    resp <- liftIO $ postWith options url [ partFileSource "image" path
                                          , partLBS "tags"  (encode tags) ]

    return $ case resp ^? responseBody . key "id" of
        Just (String code) -> Just $ ApiImageCode code
        _                  -> Nothing

-- -----------------------------------------------------------------------------

getResourceUrl :: (MonadReader env m, HasAPIConfig env) => String -> m String
getResourceUrl rsrc = ((++ rsrc) . getApiRoot) `liftM` ask

getOptions :: ( MonadReader env m, HasHttpManager env, HasAPIConfig env
              , MonadIO m)
           => m Options
getOptions = do
    env <- ask

    let httpMan = getHttpManager env
        apiKey  = getApiKey      env
        options = defaults & manager                .~ Right httpMan
                           & header "X-Mashape-Key" .~ [apiKey]

    return options
