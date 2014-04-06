module Handler.Image (
      getImagesR, postImagesR, getImageR, patchImageR, deleteImageR
    ) where

import Import

import ImageIndex.Manage
import Util.Mashape

-- | Lists every image of the user.
getImagesR :: Handler Value
getImagesR = do
    user <- mhUser <$> getMashapeHeaders
    ii <- imageIndex <$> getYesod
    currentTime <- getCurrentTime
    atomically $ do
        ui <- getUserIndex ii (userName user) currentTime
        getTagImages (uiRootTag ui)

    returnJson 

-- | Registers a new image to the index.
postImagesR :: Handler Value
postImagesR = undefined

getImageR :: Hmac -> Handler Value
getImageR = undefined

patchImageR :: Hmac -> Handler Value
patchImageR = undefined

deleteImageR :: Hmac -> Handler Value
deleteImageR = undefined
