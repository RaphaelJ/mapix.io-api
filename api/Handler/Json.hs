-- | Provides 'ToJSON' and 'FromJSON' instances for various api types.
module Handler.Json () where

import Prelude

import Control.Monad
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import Text.Parsec.Text (char, count, optional, parse, satisfy)
import Yesod.Core.Json

import Handler.Tag (tagPath)
import Handler.Type
import Histogram (Color (..), ImageWithColors (..), histColor)

instance ToJSON Image where
    toJSON Image {..} =
        object $ [
              "id"     .= iHmac
            , "tags"   .= tags
            ] ++ mName
      where
        mName | Just name <- iName = [ "name" .= name ]
              | otherwise          = []

instance ToJSON ImageWithColors where
    toJSON (ImageWithColors img) =
        let Object imgJson = toJSON img
        in Object $! H.insert "colors" (histColor iHist) imgJson

instance ToJSON (Color w) where
    toJSON (Color rgb@(RGBPixel r g b) w) =
        object [ "hex"    .= rgb2Hex rgb
               , "rgb"    .= array [r, g, b]
               , "weight" .= w ]
      where
        rgb2Hex !(RGBPixel r g b) =
            T.pack $! printf "#%s%s%s" (toHex r) (toHex g) (toHex b)

        toHex v = let (q, r) = v `quotRem` 16
                  in [ intToDigit q, intToDigit r ]

instance FromJSON w => FromJSON (Color w) where
    fromJSON (Object o) = do
        color <-     ((o .: "rgb") >>= rgb)
                 <|> ((o .: "hex") >>= hex)
        Color color <$> (o .: "weight")
      where
        rgb vec | V.length vec == 3 = let [r, g, b] = V.toList vec
                                      in return $! RGBPixel r g b
                | otherwise         = mzero

        hex s | Right color <- parse hexColorParser "" s = return color
              | otherwise                                = mzero

        hexColorParser = do
            optional (char '#')
            cs <- count 6 hexChar
            eof
            case cs of
                [a, b, c, d, e, f] ->
                    let hexValue a b = digitToInt a * 16 + digitToInt b
                    in RGBPixel (hexValue a b) (hexValue c d) (hexValue e f)
                _                  -> fail ""

        hexChar = satisfy isHexDigit
    fromJSON _          = mzero

instance ToJSON SearchResult where
    toJSON (SearchResult img score) =
        object [ "image" .= img
               , "score" .= score ]

instance ToJSON Tag where
    toJSON (Tag _ subs _) = array subs

instance ToJSON TagPath where
    toJSON = String . tagPath2Text

instance FromJSON TagPath where
    fromJSON (String s) | Right tag <- parse tagPathParser "" s = return tag
    fromJSON _                                                  = mzero

