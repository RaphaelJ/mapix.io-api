module ImageIndex.Persistent.Manage (
      getUser
    ) where

import Prelude

import Database.Persist
import Database.Persist.Sql
import Yesod.Persist

import ImageIndex.Persistent.Model
import ImageIndex.Type (UserName)

-- Users -----------------------------------------------------------------------

-- | Searches for an existing user index entry by the user name and returns it.
--
-- If a such entry doesn\'t exist, creates a new one.
getUser :: ( Monad (YesodDB site), PersistUnique (YesodDB site)
           , PersistStore  (YesodDB site)
           , PersistMonadBackend (YesodDB site) ~ SqlBackend)
        => UserName -> YesodDB site (Entity User)
getUser username = do
    mUser <- getBy $ UniqueUserName username
    case mUser of
        Just user -> return user
        Nothing   -> do
            let user = User username
            userId <- insert user
            return $! Entity userId user

