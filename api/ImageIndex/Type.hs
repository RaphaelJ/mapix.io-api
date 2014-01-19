module ImageIndex.Type where

import Data.Int
import Data.Map.Strict (Map)

data ImageIndex = ImageIndex {
      iiUsers      :: !(TVar (Map UserName      UserIndex))
    , iiHists      :: !(TVar (Map HistogramHash Histogram))
    , iiLastCalled :: !LastCalled
    }

-- Each API user has its own index. Index are used to search for image by tag or
-- by hmac.

type UserName = Text

data UserIndex = UserIndex {
      uiName    :: !UserName
    , uiRootTag :: !Tag
    , uiCount   :: !Int64
    }

data Image = Image {
      iHmac :: !Hmac
    , iName :: !(TVar (Maybe Text))
    , iTags :: !(TVar (Set Tag))
    , iHist :: !Histogram
    } deriving Ord

-- Identical histograms are shared among users. Histograms are only removed when
-- no more image links to it.

type HistogramHash = ByteString

data Histogram = Histogram {
      hHash  :: !HistogramHash
    , h5D    :: !(H.Histogram DIM5 Float)
    , h3D    :: !(H.Histogram DIM3 Float)
    , hCount :: !(TVar Int)
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

-- Here we define a concurrent double linked list which implements a Least
-- Recently Used queue for user indexes. Indexes are kept sorted by descending
-- last usage order so we can quickly remove indexes which haven't been used for
-- a month.

data LastCalled = LastCalled {
      lcFirst :: !(TVar (Maybe LastCalledNode))
    , lcLast  :: !(TVar (Maybe LastCalledNode))
    , lcMap   :: !(TVar (Map UserName LastCalledNode))
    }

data LastCalledNode = LastCalledNode {
      lcnTime :: !(TVar UTCTime)
    , lcnPrev :: !(TVar (Maybe UserIndex))
    , lcnNext :: !(TVar (Maybe UserIndex))
    }
