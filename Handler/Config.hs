module Handler.Config where

import Prelude

import Histogram (Intersec)

-- | Maximum file size of a side of an image.
confMaxFileSize :: Int
confMaxFileSize = 4 * 1024 * 1024

-- | Default number of images listed in search results.
confDefaultCount :: Int
confDefaultCount = 100

-- | Maximum number of images listed in search results.
confMaxCount :: Int
confMaxCount = 500

-- | Minimum score for matches to be listed in search results.
confMinScore :: Intersec
confMinScore = 0.05

-- | Default minimum score for matches to be listed in search results.
confDefaultMinScore :: Intersec
confDefaultMinScore = 0.05
