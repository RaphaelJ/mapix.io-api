module ImageIndex.Type where

import Data.Int
import Data.Map.Strict (Map)

data ImageIndex = ImageIndex {
      iiUsers      :: !(TVar (Map UserName UserIndex))
    , iiLastCalled :: !LastCalledQueue
    }

-- Each API user has its own index. Index are used to search for image by tag or
-- by hmac.

type UserName = Text

data UserIndex = UserIndex {
      uiName    :: !UserName
    , uiImages  :: !(TVar (Map Hmac Image))
    , uiRootTag :: !Tag
    }

-- Here we define two types of tags.
-- The RootTag is used in the UserIndex to indicate the "catch all" tag. This
-- tag doesn't have any parent and has no name (it catches the empty tag
-- string). It contains only images which aren't registered to any tag.
-- The SubTag is used for "real" tags of the hierarchy.

type TagName = Text

data Tag = RootTag | SubTag !TagName !Tag -- ^ Tag\'s name and sub-tag

data Tag = Tag {
      tType    :: !TagType
    , tSubTags :: !(TVar (Map TagName Tag))
    , tImages  :: !(TVar (Set Image))
    }

data Image = Image {
      iHmac :: !Hmac
    , iName :: !(Maybe Text)
    , iTags :: !(Set Tag)
    , iHist :: !Histogram
    } deriving Ord

data Histogram = Histogram {
    , h5D    :: !(H.Histogram DIM5 Float)
    , h3D    :: !(H.Histogram DIM3 Float)
    }

-- Here we define a concurrent double linked list which implements a Least
-- Recently Used queue for user indexes. Indexes are kept sorted by descending
-- last usage order so we can quickly remove indexes which haven't been used for
-- a month.

data LastCalledQueue = LastCalledQueue {
      lcFirst :: !(TVar (Maybe LastCalledNode)) -- ^ Most recent call.
    , lcLast  :: !(TVar (Maybe LastCalledNode)) -- ^ Least recent call.
    , lcMap   :: !(TVar (Map UserName LastCalledNode))
    }

data LastCalledNode = LastCalledNode {
      lcnUser :: !UserIndex
    , lcnTime :: !(TVar UTCTime)
    , lcnPrev :: !(TVar (Maybe LastCalledNode)) -- ^ More recent call.
    , lcnNext :: !(TVar (Maybe LastCalledNode)) -- ^ Less recent call.
    }
