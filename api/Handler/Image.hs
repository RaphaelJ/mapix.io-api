module Handler.Image (
      getImagesR, postImagesR, getImageR, patchImageR, deleteImageR
    ) where

import Import

-- | Lists every image of the user.
getImagesR :: Handler RepJson
getImagesR = undefined

-- | Registers a new image to the index.
postImagesR :: Handler RepJson
postImagesR = undefined

getImageR :: Hmac -> Handler RepJson
getImageR = undefined

patchImageR :: Hmac -> Handler RepJson
patchImageR = undefined

deleteImageR :: Hmac -> Handler RepJson
deleteImageR = undefined
