module ImageIndex.Type where

import Data.Int
import Data.Map.Strict (Map)

import Util.Hmac.Type

data ImageIndex = ImageIndex {
      iiUsers    :: !(TVar (Map UserName UserIndex))
    -- Here we define a concurrent double linked list which implements a Least
    -- Recent Call queue for user indexes. Indexes are kept sorted by descending
    -- last usage order so we can quickly remove indexes which haven't been used
    -- for a long time.
    , iiLRCFirst :: !(TVar (Maybe UserIndex)) -- ^ Most recent call.
    , iiLRCLast  :: !(TVar (Maybe UserIndex)) -- ^ Least recent call.
    }

-- Each API user has its own index. Index are used to search for image by tag or
-- by hmac.

type UserName = Text

data UserIndex = UserIndex {
      uiName    :: !UserName
    , uiImages  :: !(TVar (Map Hmac Image))
    , uiRootTag :: !Tag
    , uiLRCTime :: !UTCTime
    , uiLRCPrev :: !(TVar (Maybe UserIndex)) -- ^ More recently called user.
    , uiLRCNext :: !(TVar (Maybe UserIndex)) -- ^ Less recently called user.
    }

-- Here we define two types of tags.
-- The RootTag is used in the UserIndex to indicate the "catch all" tag. This
-- tag doesn't have any parent and has no name (it catches the empty tag
-- string). It contains only images which aren't registered to any tag.
-- The SubTag is used for "real" tags of the hierarchy.

type TagPath = [Text]

data Tag = RootTag | SubTag !Text !Tag -- ^ Tag\'s name and sub-tag

data Tag = Tag {
      tType    :: !TagType
    , tSubTags :: !(TVar (Map Text Tag))
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
