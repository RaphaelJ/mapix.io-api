module Model where

import Prelude

import Yesod
import Data.Text (Text)

import API (APIImageCode)

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

ApiImage
    apiId                       APIImageCode
    flickr                      FlickrImageId

    UniqueAPIImageAPIId         apiId
|]
