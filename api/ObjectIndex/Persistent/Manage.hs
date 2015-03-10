module ObjectIndex.Persistent.Manage (
      getUser
    , addObject, removeObject
    , removeTag
    ) where

import Prelude

import Control.Monad
import Control.Monad.IO.Class
import Database.Persist
import Database.Persist.Sql

import qualified Data.Set as S

import Histogram (ResizedImage)
import ObjectIndex.Persistent.Model
import ObjectIndex.Type (IndexedObject (..), TagPath, UserName)
import ObjectIndex.Tag (tagPath)

-- Users -----------------------------------------------------------------------

-- | Searches for an existing user index entry by the user name and returns it.
--
-- If a such entry doesn\'t exist, creates a new one.
getUser :: MonadIO m => UserName -> SqlPersistT m (Entity User)
getUser username = do
    mUser <- getBy $ UniqueUserName username
    case mUser of
        Just user -> return user
        Nothing   -> do
            let user = User username
            userId <- insert user
            return $! Entity userId user

-- Objects----------------------------------------------------------------------

-- | Inserts the object, its tags and its sources images to the database.
addObject :: MonadIO m => UserId -> IndexedObject -> [ResizedImage]
          -> SqlPersistT m (Entity Object)
addObject userId IndexedObject {..} imgs = do
    let obj = Object ioCode userId ioName ioHist
    objId <- insert obj

    insertMany_ $ map (Image objId) imgs

    -- Condition needed because of a bug in persistent when given an empty list.
    when (not $ S.null ioTags) $ do
        insertMany_ [ ObjectTag userId objId (tagPath tag)
                    | tag <- S.toList ioTags ]

    return $! Entity objId obj

removeObject :: MonadIO m => UserId -> IndexedObject -> SqlPersistT m ()
removeObject userId IndexedObject {..} =
    deleteCascadeWhere [ ObjectCode ==. ioCode, ObjectOwner ==. userId ]

-- Tags ------------------------------------------------------------------------

-- | Removes the tag without removing its associated images.
removeTag :: MonadIO m => UserId -> TagPath -> SqlPersistT m ()
removeTag userId path =
    deleteWhere [ ObjectTagOwner ==. userId, ObjectTagPath ==. path ]
