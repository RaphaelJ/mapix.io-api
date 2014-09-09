module ImageIndex.Persistent.Manage (
      getUser
    , addImage, removeImage
    , removeTag
    ) where

import Prelude

import Control.Monad
import Control.Monad.IO.Class
import Database.Persist
import qualified Data.Set as S
import Database.Persist.Sql

import ImageIndex.Persistent.Model
import ImageIndex.Type (IndexedImage (..), TagPath, UserName)
import ImageIndex.Tag (tagPath)

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

-- Images ----------------------------------------------------------------------

-- | Inserts the image and its tags to the database.
addImage :: MonadIO m => UserId -> IndexedImage -> SqlPersistT m (Entity Image)
addImage userId IndexedImage {..} = do
    let img = Image iiCode userId iiName iiHist
    imgId <- insert img

    -- Condition needed because of a bug in persistent when given an empty list.
    when (not $ S.null iiTags) $ do
        insertMany_ [ ImageTag userId imgId (tagPath tag)
                    | tag <- S.toList iiTags ]

    return $! Entity imgId img

removeImage :: MonadIO m => UserId -> IndexedImage -> SqlPersistT m ()
removeImage userId IndexedImage {..} =
    deleteCascadeWhere [ ImageCode ==. iiCode, ImageOwner ==. userId ]

-- Tags ------------------------------------------------------------------------

-- | Removes the tag without removing its associated images.
removeTag :: MonadIO m => UserId -> TagPath -> SqlPersistT m ()
removeTag userId path =
    deleteWhere [ ImageTagOwner ==. userId, ImageTagTag ==. path ]
