module ImageIndex.Persistent.Model where

import Prelude

import Data.Text (Text)
import Yesod.Persist

import Histogram ()
import ImageIndex.Instance ()
import ImageIndex.Type (IndexedHistogram, ImageCode, UserName)

share [mkPersist sqlOnlySettings, mkMigrate "migrateIndex"] [persistLowerCase|
User
    name                        UserName

    UniqueUserName              name
    deriving Show

Image
    code                        ImageCode
    owner                       UserId
    name                        Text Maybe
    histogram                   IndexedHistogram

    UniqueImageCodeOwner        code owner
    deriving Show

Tag
    owner                       UserId
    name                        Text
    parent                      TagId Maybe

    deriving Show

ImageTag
    image                       ImageId
    tag                         TagId

    deriving Show
|]
