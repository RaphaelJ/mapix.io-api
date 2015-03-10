-- | Provides various api types.
module Handler.Internal.Type where

import Data.Map (Map)
import Data.Text (Text)

import ObjectIndex (IndexedObject, Tag, TagType)

-- | The JSON instance of this 'IndexedObject' wrapper will also display the
-- main colors of the image.
newtype ObjectWithColors = ObjectWithColors IndexedObject

-- | Contains a tag and its set of subtags outside of the 'TVar'.
data StaticTag = StaticTag TagType (Map Text Tag)
