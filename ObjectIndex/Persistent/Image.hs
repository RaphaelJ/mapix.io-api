module ObjectIndex.Persistent.Image (PersistImage (..)) where

import ClassyPrelude

import Data.Serialize (Serialize)
import Database.Persist.Sql (PersistFieldSql (..))
import Vision.Image.Storage.DevIL (JPG (..), PNG (..), loadBS, saveBS)
import Yesod (
      PersistField (..), PersistValue (PersistByteString), SqlType (SqlBlob)
    )

import qualified Data.Serialize as S
import qualified Data.Text      as T

import Histogram (ResizedImage (..))

-- | An image type which can be saved to the database.
data PersistImage = PersistJPG ResizedImage -- ^ Lossy compression.
                  | PersistPNG ResizedImage -- ^ Lossless compression.

instance Serialize PersistImage where
    -- Serialize the image in an Either. Left is for JPG encoding while Right is
    -- used for PNG encoding.

    put img = do
        case img of
            PersistJPG resized ->     resizedToBs JPG resized
                                  >>= (putEither . Left)
            PersistPNG resized ->     resizedToBs PNG resized
                                  >>= (putEither . Right)
      where
        resizedToBs typ (ResizedImage io) =
            case saveBS typ io of
                Right bs -> return bs
                Left err -> fail $ show err

        -- This function is required to enforce the type of the unused part of
        -- Either.
        putEither :: S.Putter (Either ByteString ByteString)
        putEither = S.put

    get = do
        eBS <- S.get
        case eBS of
            Left  bs -> PersistJPG <$> bsToResized JPG bs
            Right bs -> PersistPNG <$> bsToResized PNG bs
      where
        bsToResized typ bs =
            case loadBS typ bs of
                Right io -> return $! ResizedImage $! io
                Left err -> fail $ show err

instance PersistField PersistImage where
    toPersistValue = PersistByteString . S.encode

    fromPersistValue ~(PersistByteString bs) =
        either (Left . T.pack) Right $ S.decode bs

instance PersistFieldSql PersistImage where
    sqlType _ = SqlBlob
