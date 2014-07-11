module Handler.Config (Config (..), defaultConfig) where

data Config {
      cMaxFileSize :: Int -- ^ Maximum file size of a side of an image.
    }

defaultConfig :: Config
defaultConfig = Config {
      cMaxFileSize = 4 * 1024 * 1024
    }
