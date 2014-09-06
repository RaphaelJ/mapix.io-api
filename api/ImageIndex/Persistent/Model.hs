module ImageIndex.Persistent.Model where

import Prelude

import Data.Text (Text)
import Yesod.Persist

import Histogram ()
import ImageIndex.Instance ()
import ImageIndex.Type (IndexedHistogram, ImageCode, TagPath, UserName)

share [ mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateIndex"] [persistLowerCase|
User
    name                        UserName

    UniqueUserName              name
    deriving Show

Image
    code                        ImageCode
    owner                       UserId
    name                        Text Maybe
    histogram                   IndexedHistogram

    UniqueImageCodeOwner        owner code
    deriving Show

ImageTag
    image                       ImageId
    tag                         TagPath

    UniqueImageTagImageTag      image tag
    deriving Show
|]
