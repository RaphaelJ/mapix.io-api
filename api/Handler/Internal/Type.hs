-- | Provides various api types.
module Handler.Internal.Type where

import Prelude

import Data.Map (Map)
import Data.Text (Text)

import ImageIndex (IndexedImage, Tag, TagType)

data SearchResult = SearchResult {
      srImage :: IndexedImage
    , srScore :: Float
    }

-- | Contains a tag and its set of subtags outside of the 'TVar'.
data StaticTag = StaticTag TagType (Map Text Tag)
