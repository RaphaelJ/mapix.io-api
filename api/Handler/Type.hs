-- | Provides various api types.
module Handler.Type () where

data SearchResult = SearchResult {
      srImage :: Image
    , srScore :: Float
    }
