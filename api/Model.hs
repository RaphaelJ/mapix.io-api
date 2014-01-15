module Model where

import Prelude
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Vision.Histogram as H
import Vision.Primitive (DIM5)
import Yesod

import Model.Field ()
import Util.Hmac.Type

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name                        Text -- Mashape User

    UniqueUserName              name
    deriving Show

-- Tracks HMACs allocations for images.
UniqueHmac
    -- Unique identifier generated by hasing the UniqueHmacId.
    value                       Hmac

    UniqueUniqueHmacValue       value
    deriving Show

Histogram
    hash                        ByteString -- SHA-1 of the histogram.
    value                       (H.Histogram DIM5 Float)
    count                       Int

    UniqueHistogramHash         hash
    deriving Show

Image
    hmac                        Hmac
    owner                       UserId
    name                        Text Maybe
    histogram                   HistogramId
    deriving Show

Tag
    owner                       UserId
    name                        Text
    parent                      TagId Maybe
    count                       Int
    deriving Show

ImageTag
    image                       ImageId
    tag                         TagId
    deriving Show
|]
