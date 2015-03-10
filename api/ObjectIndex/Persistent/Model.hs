module ObjectIndex.Persistent.Model where

import Prelude

import Data.Text (Text)
import Yesod.Persist

import Histogram (ResizedImage)
import ObjectIndex.Instance ()
import ObjectIndex.Type (IndexedHistogram, ObjectCode, TagPath, UserName)

share [ mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateIndex"] [persistLowerCase|
User
    name                        UserName

    UniqueUserName              name
    deriving Show

Object
    code                        ObjectCode
    owner                       UserId
    name                        Text Maybe
    histogram                   IndexedHistogram

    UniqueObjectOwnerCode       owner code
    deriving Show

-- Raw images which have been used to compute the histograms.
Image
    object                      ObjectId
    image                       ResizedImage

ObjectTag
    owner                       UserId
    object                      ObjectId
    path                        TagPath

    UniqueObjectTagOwnerObjectPath owner object path
    deriving Show
|]
