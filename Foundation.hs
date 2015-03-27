module Foundation where

import Prelude

import qualified Data.ByteString.Lazy as B
import Database.Persist.Sql (SqlBackend)
import Network.HTTP.Conduit (Manager)
import Yesod
import Yesod.Core.Types (Logger)
import Yesod.Default.Config

import ObjectIndex (ObjectIndex, ObjectCode, TagPath)
import Settings (Extra (..))
import Settings.Development (development)

import qualified Handler.Error as E
import qualified Settings

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App {
      settings      :: AppConfig DefaultEnv Extra
    , connPool      :: PersistConfigPool Settings.PersistConf
    , httpManager   :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger     :: Logger
    , encryptKey    :: B.ByteString
    , objectIndex   :: ObjectIndex
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    errorHandler err =
        E.apiFail $ case err of
                        NotFound           -> E.NotFound
                        InternalError msg  -> E.InternalServerError msg
                        InvalidArgs msgs   -> E.BadRequest msgs
                        NotAuthenticated   -> undefined
                        PermissionDenied _ -> undefined
                        BadMethod method   -> E.MethodNotAllowed method

    makeSessionBackend _ = return Nothing

    makeLogger = return . appLogger

    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB = defaultRunDB persistConfig connPool

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod
