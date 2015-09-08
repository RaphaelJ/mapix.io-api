module ObjectIndex.Persistent.Restore (restoreIndex) where

import ClassyPrelude

import Control.Monad.IO.Class
import Database.Persist
import Database.Persist.Sql

import qualified Data.Set as S

import ObjectIndex.Manage (addObject, getTag, getUserIndex, runTransaction)
import ObjectIndex.Persistent.Model
import ObjectIndex.Type (ObjectIndex, UserName)

data FreezedUserIndex = FreezedUserIndex {
      fuiName    :: !UserName
    , fuiObjects :: ![Object]
    }

-- | Restores an 'ObjectIndex' from the current state of the database.
restoreIndex :: (MonadIO m, Functor m) => ObjectIndex -> SqlPersistT m ()
restoreIndex ii = do
    users <- selectList [] []

    -- Loads the entire index in an immutable structure.
    fuis <- forM users $ \(Entity userId (User username)) -> do
        objs <- selectList [ ObjectOwner ==. userId ] []
        return $! FreezedUserIndex username (map entityVal objs)

    -- Pushs the whole immutable index in the transactional index in a single
    -- STM transaction.

    runTransaction $ do
        forM_ fuis $ \(FreezedUserIndex {..}) -> do
            ui <- getUserIndex ii fuiName

            forM_ fuiObjects $ \(Object {..}) -> do
                tags <- S.fromList <$> mapM (getTag ui) objectTags
                addObject ui objectCode objectName tags objectHistogram
