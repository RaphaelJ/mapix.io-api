module Model where

import Prelude
import Data.Text (Text)
import qualified Vision.Histogram as H
import Vision.Primitive (DIM3)
import Yesod

import ImageIndex (ImageCode)
import Histogram.Serialize ()

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name                        Text -- Mashape User

    UniqueUserName              name
    deriving Show

Image
    code                        ImageCode
    owner                       UserId
    name                        Text Maybe
    histogram                   (H.Histogram DIM3 Float)
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
