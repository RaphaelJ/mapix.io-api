-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import Prelude

import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.Sqlite (SqliteConf)
import Yesod.Default.Config
import Yesod.Default.Util
import Data.Text (Text)
import Data.Yaml
import Settings.Development
import Data.Default (def)
import Text.Hamlet

import API (HasAPIConfig (..))

-- | Which Persistent backend this site is using.
type PersistConf = SqliteConf

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: FilePath
staticDir = "static"

staticRoot :: AppConfig DefaultEnv x -> Text
staticRoot conf = [st|#{appRoot conf}/static|]

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if development then widgetFileReload
                             else widgetFileNoReload)
              widgetFileSettings

data Extra = Extra {
      eApiRoot :: Text
    , eApiKey  :: Text
    } deriving Show

instance HasAPIConfig Extra where
    getApiRoot = eApiRoot
    getApiKey  = eApiKey

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .: "apiroot"
    <*> o .: "apikey"
