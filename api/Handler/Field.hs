module Handler.Field (
      
    ) where

import Control.Monad
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)

countField = checkBool (> 0) "Non-positive count value" intField

-- | Fails if at least one image is unreadable.
imagesField =
    Field {
          fieldParse = \_ files -> do
                mImgs <- readImages files
                case mImgs of
                    Just _  -> Right mImgs
                    Nothing -> Left "Unreadable image"
        , fieldView = undefined, fieldEnctype = Multipart
        }
  where
    readImages files = runMaybeT $ do
        forM files $ \file -> do
            bs   <- runResourceT $ fileSourceRaw file $$ sinkLbs
            eImg <- liftIO $ I.loadBS Nothing bs

            case eImg of Left  _   -> mzero
                         Right img -> return img
