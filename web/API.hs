{-# LANGUAGE OverloadedStrings #-}

module API (
      APIImageCode (..), Tag, HasAPIConfig (..)
    , postImage
    , getResourceUrl, getOptions, withMashapeKey
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

newtype APIImageCode = APIImageCode { aicValue :: Text }
    deriving (Eq, Ord, Read, PersistField)

instance PersistFieldSql APIImageCode where
    sqlType action = sqlType (aicValue `liftM` action)

type Tag = Text

class HasAPIConfig a where
    getApiRoot :: a -> String
    getApiKey  :: a -> ByteString

postImage :: ( MonadReader env m, HasHttpManager env, HasAPIConfig env
             , MonadIO m)
          => FilePath -> [Tag] -> m (Maybe APIImageCode)
postImage path tags = do
    options <- getOptions
    url     <- getResourceUrl "/images"

    resp <- liftIO $ postWith options url [ partFileSource "image" path
                                          , partLBS "tags"  (encode tags) ]

    return $ case resp ^? responseBody . key "id" of
        Just (String code) -> Just $ APIImageCode code
        _                  -> Nothing

-- -----------------------------------------------------------------------------

getResourceUrl :: (MonadReader env m, HasAPIConfig env) => String -> m String
getResourceUrl rsrc = ((++ rsrc) . getApiRoot) `liftM` ask

-- | Creates an 'Options' object from the monad HTTP 'Manager' which can be used
-- to query the API.
getOptions :: ( MonadReader env m, HasHttpManager env, HasAPIConfig env
              , MonadIO m)
           => m Options
getOptions = do
    env <- ask

    let httpMan = getHttpManager env
        options = defaults & manager .~ Right httpMan

    withMashapeKey options

-- | Adds the X-Mashape-Key header with the Monad's API key to every request
-- which will be made with the returned 'Options' object.
withMashapeKey :: (HasAPIConfig env, MonadReader env m) => Options -> m Options
withMashapeKey options = do
    env <- ask

    let apiKey   = getApiKey env
        options' = options & header "X-Mashape-Key" .~ [apiKey]

    return options'
