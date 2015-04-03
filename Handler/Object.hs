module Handler.Object (
      getObjectsR, postObjectsR, deleteObjectsR
    , getObjectR, deleteObjectR
    ) where

import Import

import Control.Monad
import Data.Maybe
import Network.HTTP.Types.Status (created201, noContent204)
import System.Random (newStdGen)

import qualified Data.Foldable  as F
import qualified Data.Set       as S

import Handler.Error (APIError (IndexExhausted), apiFail)
import Handler.Internal.Form (
      filterForm, imagesField, jsonField, tagExpressionField
    )
import Handler.Internal.Json ()
import Handler.Internal.Listing (listing, listingForm)
import Handler.Internal.Mashape (
      getMashapeHeaders, maxIndexSize, mhSubscription, mhUser
    )
import Handler.Internal.Type (IndexedObjectWithColors (..))
import Histogram (ResizedImage, fromImages)
import ObjectIndex (
      ObjectCode, IndexedObject (..), TagPath
    , getMatchingObjects, getTag, getUserIndex, lookupObject, newObject
    , removeObject, runTransaction, touchUserIndex, userIndexSize
    )

import qualified ObjectIndex.Persistent as DB

-- | Lists every object in the user's index.
getObjectsR :: Handler Value
getObjectsR = do
    listingParams <- runInputGet listingForm
    tagExpr       <- runInputGet filterForm

    username <- mhUser <$> getMashapeHeaders
    ii       <- objectIndex <$> getYesod

    objs <- runTransaction $ do
        ui <- getUserIndex ii username
        getMatchingObjects ui tagExpr

    returnJson $ listing listingParams (Just $! S.size objs) (S.toList objs)

data NewObject = NewObject {
      noName       :: Maybe Text
    , noImages     :: [(ResizedImage, Bool)]
    , noTags       :: Maybe [TagPath]
    , noIgnoreBack :: Bool
    , noIgnoreSkin :: Bool
    }

-- | Registers a new object to the index.
--
-- Returns a '201 Created' status on success. Fails with a '400 Bad request'
-- with an invalid query or a '429 Too Many Requests'.
postObjectsR :: Handler Value
postObjectsR = do
    NewObject {..} <- runInputPost newObjectForm

    let !hist = fromImages noIgnoreBack noIgnoreSkin (map fst noImages)

    headers <- getMashapeHeaders
    let username = mhUser headers
        maxSize  = maxIndexSize $ mhSubscription headers

    app     <- getYesod
    let key = encryptKey app
        oi  = objectIndex app

    gen     <- lift newStdGen

    -- Tries to add the object. Returns Nothing if the index has too
    -- many objects.
    mObj <- runDB $ do
        mObj <- runTransaction $ do
            ui   <- getUserIndex oi username
            size <- userIndexSize ui

            let indexIsFull = maybe False (size >=) maxSize

            if indexIsFull
                then return Nothing
                else do
                    tags     <- mapM (getTag ui) (fromMaybe [] noTags)
                    (obj, _) <- newObject key ui gen noName (S.fromList tags)
                                          hist
                    touchUserIndex oi ui
                    return $! Just obj

        -- If the in-memory transaction succeed, saves the object in the
        -- persistent database.
        case mObj of
            Just obj -> do
                Entity userId _ <- DB.getUser username
                _ <- DB.addObject userId obj noImages
                return $ Just obj
            Nothing  -> return Nothing

    case mObj of
        Just obj -> do
            url <- getUrlRender <*> pure (ObjectR $! ioCode obj)
            addHeader "Location" url
            sendResponseStatus created201 (toJSON $ IndexedObjectWithColors obj)
        Nothing  -> apiFail IndexExhausted
  where
    newObjectForm = NewObject <$> iopt textField     "name"
                              <*> ireq imagesField   "images"
                              <*> iopt tagsField     "tags"
                              <*> ireq checkBoxField "ignore_background"
                              <*> ireq checkBoxField "ignore_skin"

    tagsField = jsonField "Invalid tag list"

-- | Deletes every object matching the (optional) tag expression.
--
-- Returns a '204 No Content'.
deleteObjectsR :: Handler ()
deleteObjectsR = do
    tagExpr <- runInputGet (iopt tagExpressionField "filter")

    username <- mhUser <$> getMashapeHeaders
    oi       <- objectIndex <$> getYesod

    runDB $ do
        objs <- runTransaction $ do
            ui   <- getUserIndex oi username
            objs <- getMatchingObjects ui tagExpr
            F.mapM_ (removeObject ui) objs
            return objs

        when (not $ S.null objs) $ do
            Entity userId _ <- DB.getUser username
            F.mapM_ (DB.removeObject userId) objs

    sendResponseStatus noContent204 ()

-- | Returns the data associated with an object.
--
-- Fails with a '404 Not found' error when the object is not in the index.
getObjectR :: ObjectCode -> Handler Value
getObjectR code = do
    username    <- mhUser <$> getMashapeHeaders
    oi          <- objectIndex <$> getYesod

    mObj <- runTransaction $ do
        ui <- getUserIndex oi username
        lookupObject ui code

    case mObj of Just obj -> returnJson $ IndexedObjectWithColors obj
                 Nothing  -> notFound

-- | Returns a '204 No content' on success. Fails with a '404 Not found' error
-- when the object is not in the index.
deleteObjectR :: ObjectCode -> Handler ()
deleteObjectR code = do
    username <- mhUser <$> getMashapeHeaders
    oi       <- objectIndex <$> getYesod

    exists <- runDB $ do
        mObj <- runTransaction $ do
            ui   <- getUserIndex oi username

            mObj <- lookupObject ui code
            case mObj of Just obj -> removeObject ui obj
                         Nothing  -> mzero

            return mObj

        case mObj of
            Just obj -> do
                Entity userId _ <- DB.getUser username
                DB.removeObject userId obj
                return True
            Nothing  -> return False

    if exists then sendResponseStatus noContent204 ()
              else notFound
