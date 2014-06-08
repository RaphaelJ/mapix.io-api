module ImageIndex.Config (Config (..), defaultConfig) where

import Vision.Primitive (Z (..), (:.) (..), DIM3, ix3)

data Config {
      cHistSize     :: DIM3
    }

defaultConfig :: Config
defaultConfig = Config {
      cHistSize     = ix3 8 4 4
    }
