module ImageIndex.Config (Config (..), defaultConfig) where

import Vision.Primitive (Z (..), (:.) (..), DIM3, ix3)

data Config {
      cMaxImageSize :: Int  -- Maximum size of a side of an image.
    , cHistSize     :: DIM3
    }

defaultConfig :: Config
defaultConfig = Config {
      cMaxImageSize = 480
    , cHistSize     = ix3 8 4 4
    }
