module ImageIndex.Persistent.Restore (restore) where

import Prelude

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import Database.Persist
import Database.Persist.Sql
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Yesod.Persist

import ImageIndex.Manage (addImage, getTag, getUserIndex, newIndex)
import ImageIndex.Persistent.Model
import ImageIndex.Type (
      ImageIndex, ImageCode, IndexedHistogram, UserName, TagPath
    )

data FreezedUserIndex = FreezedUserIndex {
      fuiName    :: !UserName
    , fuiImages  :: ![FreezedIndexedImage]
    }

data FreezedIndexedImage = FreezedIndexedImage {
      fiiCode :: !ImageCode
    , fiiName :: !(Maybe Text)
    , fiiTags :: ![TagPath]
    , fiiHist :: !IndexedHistogram
    }

-- | Creates a new 'ImageIndex' from the current state of the database.
restore :: ( MonadIO (YesodDB site), Functor (YesodDB site)
           , PersistUnique (YesodDB site), PersistQuery (YesodDB site)
           , PersistMonadBackend (YesodDB site) ~ SqlBackend)
        => YesodDB site ImageIndex
restore = do
    users <- selectList [] []

    -- Loads the entire index in an immutable strucute.
    fuis <- forM users $ \(Entity userId (User username)) -> do
        imgs <- selectList [ImageOwner ==. userId] []

        fiis <- forM imgs $ \(Entity imgId (Image code _ name hist)) -> do
            tags <-     map (imageTagTag . entityVal)
                    <$> selectList [ImageTagImage ==. imgId] []

            return $! FreezedIndexedImage code name tags hist

        return $! FreezedUserIndex username fiis

    -- Pushs the whole immutable index in the transactional index in a single
    -- transaction.

    liftIO $ do
        ii          <- newIndex
        currentTime <- getCurrentTime

        atomically $ do
            forM_ fuis $ \(FreezedUserIndex {..}) -> do
                ui <- getUserIndex ii fuiName currentTime

                forM_ fuiImages $ \(FreezedIndexedImage {..}) -> do
                    tags <- mapM (getTag ui) fiiTags
                    addImage ui fiiCode fiiName tags fiiHist

            return ii
