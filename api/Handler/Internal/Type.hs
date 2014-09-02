-- | Provides various api types.
module Handler.Internal.Type where

import Data.Map (Map)
import Data.Text (Text)

import ImageIndex (IndexedImage, Tag, TagType)

-- | The JSON instance of this 'IndexedImage' wrapper will also display the main
-- colors of the image.
newtype ImageWithColors = ImageWithColors IndexedImage

-- | Contains a tag and its set of subtags outside of the 'TVar'.
data StaticTag = StaticTag TagType (Map Text Tag)
