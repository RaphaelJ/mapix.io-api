module Model where

import Prelude

import Yesod
import Data.Text (Text)

share [mkPersist sqlSettings, mkMigrate "migrateFlickr"] [persistLowerCase|
FlickrImage
    photoId                     Text
    title                       Text
    url                         Text
    width                       Int
    height                      Int

    UniqueFlickrImagePhotoId    photoId
    deriving Show

FlickrTag
    name                        Text

    UniqueFlickrTagName         name
    deriving Show

FlickrImageTag
    image                       FlickrImageId
    tag                         FlickrTagId

    UniqueFlickrImageTag        image tag
    deriving Show

APIImage
    apiId                       Text
    flickr                      FlickrImageId

    UniqueAPIImageAPIId         apiId
|]