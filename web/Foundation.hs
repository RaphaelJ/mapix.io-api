module Foundation where

import Prelude

import Yesod
import Yesod.Static
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import qualified Database.Persist
import Database.Persist.Sql (SqlBackend)
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Yesod.Core.Types (Logger)

import API (HasAPIConfig (..))
import qualified Settings
import Settings (widgetFile, Extra (..))
import Settings.Development (development)
import Settings.StaticFiles

data App = App
    { settings      :: AppConfig DefaultEnv Extra
    , getStatic     :: Static -- ^ Settings for static file serving.
    , connPool      :: Database.Persist.PersistConfigPool Settings.PersistConf
    , httpManager   :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger     :: Logger
    }

instance HasHttpManager App where
    getHttpManager = httpManager

instance HasAPIConfig App where
    getApiRoot = getApiRoot . appExtra . settings
    getApiKey  = getApiKey  . appExtra . settings

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    defaultLayout widget = do
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                , css_bootstrap_css
                , css_flat_ui_css
                ])

            $(combineScripts 'StaticR
                [ js_bootstrap_min_js
                , js_html5shiv_js
                , js_jquery_min_js
                , js_respond_min_js
                ])

            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y))
                       (renderRoute s)
    urlRenderOverride _ _ = Nothing

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir 
                                 (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

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
