module ImageIndex.Type where

import Data.Map.Strict (Map)

type UserName = Text

type HistogramHash = ByteString

data ImageIndex = ImageIndex {
      iiUsers   :: !(TVar (Map UserName      UserIndex))
    , iiHists   :: !(TVar (Map HistogramHash Histogram))
    }

type TagName = Text

data UserIndex = UserIndex {
      uiRootTag :: !Tag
    , uiImages  :: !(TVar (Map Hmac Image))
    }

-- Here we define two types of tags. The RootTag is used in the UserIndex to
-- indicate the "catch all" tag. This tag doesn't have any parent and has no
-- name (it catches the empty tag string). Its image set contains every image
-- which has not been tagged. The SubTag is used for "real" tags of the
-- hierarchy.

data Tag = RootTag
         | SubTag !TagName !Tag -- ^ Tag\'s name and sub-tag

data Tag = Tag {
      tType    :: !TagType
    , tSubTags :: !(TVar (Map TagName Tag))
    , tImages  :: !(TVar (Set Image))
    }

data Histogram = Histogram {
      hHash     :: !HistogramHash
    , h5D       :: !(H.Histogram DIM5 Float)
    , h3D       :: !(H.Histogram DIM3 Float)
    , hCount    :: !(TVar Int)
    }

data Image = Image {
      iHmac     :: !Hmac
    , iName     :: !(TVar (Maybe Text))
    , iTags     :: !(TVar (Set Tag))
    , iHist     :: !Histogram
    } deriving Ord
