module Foundation where

import Prelude
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import Database.Persist.Sql (SqlPersistT)
import Network.HTTP.Conduit (Manager)
import Yesod
import Yesod.Core.Types (Logger)
import Yesod.Default.Config

import ImageIndex.Type (ImageIndex)
import Settings (Extra (..))
import qualified Settings
import Settings.Development (development)

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
    , imageIndex    :: ImageIndex
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    makeSessionBackend _ = return Nothing

    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod
