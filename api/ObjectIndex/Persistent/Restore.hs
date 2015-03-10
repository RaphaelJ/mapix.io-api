module ObjectIndex.Persistent.Restore (restoreIndex) where

import Prelude

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Database.Persist
import Database.Persist.Sql
import Data.Text (Text)

import ObjectIndex.Manage (addObject, getTag, getUserIndex, runTransaction)
import ObjectIndex.Persistent.Model
import ObjectIndex.Type (
      ObjectIndex, ObjectCode, IndexedHistogram, UserName, TagPath
    )

data FreezedUserIndex = FreezedUserIndex {
      fuiName    :: !UserName
    , fuiObjects :: ![FreezedIndexedObject]
    }

data FreezedIndexedObject = FreezedIndexedObject {
      fioCode :: !ObjectCode
    , fioName :: !(Maybe Text)
    , fioTags :: ![TagPath]
    , fioHist :: !IndexedHistogram
    }

-- | Restores an 'ObjectIndex' from the current state of the database.
restoreIndex :: (MonadIO m, Functor m) => ObjectIndex -> SqlPersistT m ()
restoreIndex ii = do
    users <- selectList [] []

    -- Loads the entire index in an immutable structure.
    fuis <- forM users $ \(Entity userId (User username)) -> do
        objs <- selectList [ ObjectOwner ==. userId ] []

        fiis <- forM objs $ \(Entity objId (Object code _ name hist)) -> do
            tags <-     map (objectTagPath . entityVal)
                    <$> selectList [ ObjectTagObject ==. objId ] []

            return $! FreezedIndexedObject code name tags hist

        return $! FreezedUserIndex username fiis

    -- Pushs the whole immutable index in the transactional index in a single
    -- STM transaction.

    runTransaction $ do
        forM_ fuis $ \(FreezedUserIndex {..}) -> do
            ui <- getUserIndex ii fuiName

            forM_ fuiObjects $ \(FreezedIndexedObject {..}) -> do
                tags <- mapM (getTag ui) fioTags
                addObject ui fioCode fioName tags fioHist
