{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Provides 'ToJSON' and 'FromJSON' instances for various API types.
module Handler.Internal.Json () where

import Prelude

import Control.Applicative ((<$>), (<|>))
import Control.Monad
import Data.Aeson.Types (Parser)
import Data.Char (digitToInt, intToDigit, isHexDigit)
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word
import Text.Parsec (char, count, eof, optional, parse, satisfy)
import Text.Printf
import Vision.Image (RGBPixel (..))
import Yesod.Core.Json

import Handler.Config (confDefaultMinScore)
import Handler.Internal.Type
import ObjectIndex (
      IndexedObject (..), TagPath (..), TagType (..), SearchResult (..)
    , tagPath, tagPath2Text, tagPathParser
    )
import Histogram (Color (..), toColors)

instance ToJSON IndexedObject where
    toJSON IndexedImage {..} =
        object $ [
              "id"   .= iiCode
            , "tags" .= map tagPath (S.toList iiTags)
            ] ++ mName
      where
        mName = maybe [] (\name -> [ "name" .= name ]) iiName

instance ToJSON IndexedObjectWithColors where
    toJSON (IndexedObjectWithColors obj) =
        let Object objJson = toJSON obj
            colors         = toColors (ioHist obj) confDefaultMinScore
        in Object $! H.insert "colors" (toJSON colors) objJson

instance ToJSON Color where
    toJSON (Color (RGBPixel r g b) w) =
        object [ "hex"    .= hex
               , "rgb"    .= array [r, g, b]
               , "weight" .= w ]
      where
        hex = T.pack $! printf "#%s%s%s" (toHex r) (toHex g) (toHex b)

        toHex v = let (d, m) = int v `divMod` 16
                  in [ intToDigit d, intToDigit m ]

instance FromJSON Color where
    parseJSON (Object o) = do
        color <-     ((o .: "rgb") >>= rgb)
                 <|> ((o .: "hex") >>= hex)
        Color color <$> (o .: "weight")
      where
        rgb vec | V.length vec == 3 = let [r, g, b] = V.toList vec
                                      in return $! RGBPixel r g b
                | otherwise         = mzero

        hex :: Text -> Parser RGBPixel
        hex s | Right color <- parse hexColorParser "" s = return color
              | otherwise                                = mzero

        hexColorParser = do
            optional (char '#')
            cs <- count 6 hexChar
            eof
            case cs of
                [a, b, c, d, e, f] ->
                    let hexValue x y = word8 $ digitToInt x * 16 + digitToInt y
                        pix          = RGBPixel (hexValue a b) (hexValue c d)
                                                (hexValue e f)
                    in return pix
                _                  -> fail ""

        hexChar = satisfy isHexDigit
    parseJSON _          = mzero

instance ToJSON s => ToJSON (SearchResult s) where
    toJSON (SearchResult obj score) =
        object [ "object" .= obj
               , "score"  .= score ]

instance ToJSON StaticTag where
    toJSON (StaticTag typ subs) =
        object [ "name"     .= tagName
               , "sub-tags" .= M.keys subs ]
      where
        tagName = case typ of RootTag       -> ""
                              SubTag name _ -> name

instance ToJSON TagPath where
    toJSON = String . tagPath2Text

instance FromJSON TagPath where
    parseJSON (String s) | Right tag <- parse tagPathParser "" s = return tag
    parseJSON _                                                  = mzero

int :: Integral a => a -> Int
int = fromIntegral

word8 :: Integral a => a -> Word8
word8 = fromIntegral
