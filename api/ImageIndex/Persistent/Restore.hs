module ImageIndex.Persistent.Restore (restoreIndex) where

import Prelude

import Control.Applicative
import Control.Monad
import Database.Persist
import Database.Persist.Sql
import Data.Text (Text)

import ImageIndex.Manage (addImage, getTag, getUserIndex, runTransaction)
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

-- | Restores an 'ImageIndex' from the current state of the database.
restoreIndex :: ( PersistQuery m, Functor m
                , PersistMonadBackend m ~ SqlBackend)
             =>  ImageIndex -> m ()
restoreIndex ii = do
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

    runTransaction $ do
        forM_ fuis $ \(FreezedUserIndex {..}) -> do
            ui <- getUserIndex ii fuiName

            forM_ fuiImages $ \(FreezedIndexedImage {..}) -> do
                tags <- mapM (getTag ui) fiiTags
                addImage ui fiiCode fiiName tags fiiHist
