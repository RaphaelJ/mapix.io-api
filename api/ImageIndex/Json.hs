-- | Provides 'ToJSON' instances for image index types.
module ImageIndex.Json () where

import Prelude
import qualified Data.Set as S
import Yesod.Core.Json

import ImageIndex.Tag (tagPath)
import ImageIndex.Type

instance ToJSON Image where
    toJSON Image {..} =
        object $ [
              "id"     .= iHmac
            , "tags"   .= array [ tagPath tag
                                | tag@(Tag (SubTag _ _) _ _) <- S.elems tags ]
            , "colors" .= 
            ] ++ mName
      where
        mName | Just name <- iName = [ "name" .= name ]
              | otherwise          = []
