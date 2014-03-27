module ImageIndex.Config (Config (..), defaultConfig) where

import Vision.Primitive (Z (..), (:.) (..), DIM3, DIM5)

data Config {
      cHistSize     :: DIM3
    , cHist2DShapeR :: DIM5
    }

defaultConfig :: Config
defaultConfig = Config {
      cHistSize     = Z :. 8 :. 4 :. 4
    , cHist2DShapeR = cHistSize defaultConfig :. 3 :. 3
    }
