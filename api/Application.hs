{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application (
      makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import

import Control.Monad.Logger (runLoggingT)
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.Default (def)
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Network.HTTP.Conduit (newManager, conduitManagerSettings)
import Network.Wai.Logger (clockDateCacher)
import Network.Wai.Middleware.RequestLogger (
      mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..)
    , destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import System.Log.FastLogger (newLoggerSet, defaultBufSize)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Web.ClientSession (defaultKeyFile, randomKey)
import Yesod.Core.Types (loggerSet, Logger (Logger))
import Yesod.Default.Config
import Yesod.Default.Main

import Settings

import Handler.Image
import Handler.Search
import Handler.Tag

import ImageIndex (newIndex)
import ImageIndex.Persistent (migrateIndex)

mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    return $ logWare app

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager conduitManagerSettings
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    loggerSet' <- newLoggerSet defaultBufSize Nothing
    (getter, _) <- clockDateCacher

    key <- getEncryptionKey

    ii <- newIndex
    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App conf p manager dbconf logger key ii

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateIndex) p)
        (messageLoggerSource foundation logger)

    return foundation

-- | Tries to read the key file or initializes it with a random key.
getEncryptionKey :: IO LBS.ByteString
getEncryptionKey = do
    exists <- doesFileExist encryptKeyFile
    if exists
        then LBS.readFile encryptKeyFile
        else do
            (bs, _) <- randomKey
            SBS.writeFile encryptKeyFile bs
            return $ LBS.fromStrict bs
  where
    encryptKeyFile = "config" </> defaultKeyFile

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
