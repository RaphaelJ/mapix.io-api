-- | Provides various api types.
module Handler.Type where

import Prelude

import ImageIndex (Image)

data SearchResult = SearchResult {
      srImage :: Image
    , srScore :: Float
    }
