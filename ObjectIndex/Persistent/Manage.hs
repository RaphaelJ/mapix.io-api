module ObjectIndex.Persistent.Manage (
      getUser
    , addObject, removeObject
    ) where

import ClassyPrelude

import Database.Persist
import Database.Persist.Sql

import qualified Data.Set as S

import Histogram (ResizedImage)
import ObjectIndex.Persistent.Image (PersistImage (..))
import ObjectIndex.Persistent.Model
import ObjectIndex.Type (IndexedObject (..), UserName)
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
addObject :: MonadIO m => UserId -> IndexedObject
          -> [(ResizedImage, Bool)] -- ^ True for each image which must be saved
                                    -- as a JPG image.
          -> SqlPersistT m (Entity Object)
addObject userId IndexedObject {..} imgs = do
    let tags = map tagPath $ S.toList ioTags
        obj  = Object ioCode userId ioName tags ioHist
    objId <- insert obj

    insertMany_ [ Image objId persistImg
                | (img, isJPG) <- imgs
                , let persistImg | isJPG     = PersistJPG img
                                 | otherwise = PersistPNG img ]

    return $! Entity objId obj

removeObject :: MonadIO m => UserId -> IndexedObject -> SqlPersistT m ()
removeObject userId IndexedObject {..} =
    deleteCascadeWhere [ ObjectCode ==. ioCode, ObjectOwner ==. userId ]
