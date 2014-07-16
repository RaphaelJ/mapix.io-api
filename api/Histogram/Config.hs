module Histogram.Config (Config (..), defaultConfig) where

import Prelude

import Vision.Primitive (DIM3, ix3)

data Config = Config {
      cMaxImageSize :: Int  -- ^ Maximum size of a side of an image.
    , cHistSize     :: DIM3
    }

defaultConfig :: Config
defaultConfig = Config {
      cMaxImageSize = 480
    , cHistSize     = ix3 8 4 4
    }
