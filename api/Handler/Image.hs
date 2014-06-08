module Handler.Image (
      getImagesR, postImagesR, getImageR, patchImageR, deleteImageR
    ) where

import Import
import Data.Time.Clock (getCurrentTime)

import ImageIndex.Manage
import Util.Mashape

-- | Lists every image of the user.
getImagesR :: Handler Value
getImagesR = do
    user        <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime
    tagExpr     <- getTagExpression

    img <- atomically $ do
        ui <- getUserIndex ii (userName user) currentTime
        getMatchingImages ui tagExpr

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
