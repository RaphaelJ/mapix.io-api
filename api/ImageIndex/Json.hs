-- | Provides 'ToJSON' instances for image index types.
module ImageIndex.Json () where

import Prelude
import Control.Monad
import qualified Data.Set as S
import Yesod.Core.Json

import ImageIndex.Tag (tagPath)
import ImageIndex.Type

instance ToJSON Image where
    toJSON Image {..} =
        object $ [
              "id"     .= iHmac
            , "tags"   .= array [ tagPath tag
                                | tag@(Tag (SubTag _ _) _ _) <- S.elems tags ]
            , "colors" .= histColor iHist
            ] ++ mName
      where
        mName | Just name <- iName = [ "name" .= name ]
              | otherwise          = []

instance ToJSON (Color w) where
    toJSON (Color rgb@(RGBPixel r g b) w) =
        object [ "hex"    .= rgb2Hex rgb
               , "rgb"    .= array [r, g, b]
               , "weight" .= w ]

instance FromJSON w => FromJSON (Color w) where
    fromJSON (Object o) = do
        color <-     (o .: "rgb")
                 <|> (o .: "hex")
        Color color <$> fromJSON
    fromJSON _          = mzero
