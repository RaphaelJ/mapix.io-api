module Handler.Image (
      getImagesR, postImagesR, getImageR, patchImageR, deleteImageR
    ) where

import Import

import ImageIndex.Manage
import Util.Mashape

-- | Lists every image of the user.
getImagesR :: Handler RepJson
getImagesR = do
    user <- mhUser <$> getMashapeHeaders
    ii <- imageIndex <$> getYesod
    currentTime <- getCurrentTime
    atomically $ do
        ui <- getUserIndex ii (userName user) currentTime
        getTagImages (uiRootTag ui)

-- | Registers a new image to the index.
postImagesR :: Handler RepJson
postImagesR = undefined

getImageR :: Hmac -> Handler RepJson
getImageR = undefined

patchImageR :: Hmac -> Handler RepJson
patchImageR = undefined

deleteImageR :: Hmac -> Handler RepJson
deleteImageR = undefined
