module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

import ObjectIndex (ObjectIndex, ObjectCode, TagPath)

import qualified Handler.Error  as E

data App = App
    { appSettings    :: AppConfig DefaultEnv Extra
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appEncryptKey  :: B.ByteString
    , appObjectIndex :: ObjectIndex
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance Yesod App where
    approot = ApprootMaster $ appRoot . appSettings

    errorHandler err =
        E.apiFail $ case err of
                        NotFound           -> E.NotFound
                        InternalError msg  -> E.InternalServerError msg
                        InvalidArgs msgs   -> E.BadRequest msgs
                        NotAuthenticated   -> undefined
                        PermissionDenied _ -> undefined
                        BadMethod method   -> E.MethodNotAllowed method

    makeSessionBackend _ = return Nothing

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
