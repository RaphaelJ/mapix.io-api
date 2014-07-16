-- | Provides instances for the types from 'ImageIndex.Type'.
module ImageIndex.Instance () where

import Prelude

import Data.Aeson

import ImageIndex.Type

instance Show ImageCode where
    show (ImageCode txt) = show txt

instance Read ImageCode where
    readsPrec n str = map (first ImageCode) (readsPrec n str)
    readList str = map (first (map ImageCode)) (readList str)

instance ToJSON TagPath where
    toJSON = String . tagPath2Text

instance FromJSON TagPath where
    fromJSON (String s) | Right tag <- parse tagPathParser "" s = return tag
    fromJSON _                                                  = mzero

instance Eq Tag where
    (==)    = (==)    `on` tType

instance Ord Tag where
    compare = compare `on` tType

instance ToJSON Tag where
    toJSON (Tag _ subs _) = array subs

instance ToJSON Image where
    toJSON Image {..} =
        object $ [
              "id"     .= iHmac
            , "tags"   .= tags
            , "colors" .= histColor iHist
            ] ++ mName
      where
        mName | Just name <- iName = [ "name" .= name ]
              | otherwise          = []
