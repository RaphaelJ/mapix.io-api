module ImageIndex.Type where

import Prelude

import Control.Concurrent.STM.TVar (TVar)
import Data.Function
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.String
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Yesod

import Histogram (HeterogeneousHistogram)

-- | ImageCodes are unique randomly generated identifiers used to identify
-- images.
newtype ImageCode = ImageCode { icValue :: Text }
    deriving (Eq, Ord, IsString, PersistField, PathPiece, ToJSON)

data ImageIndex = ImageIndex {
      iiUsers     :: !(TVar (Map UserName UserIndex))
    -- Here we define a concurrent double linked list which implements a Least
    -- Recent Call queue for user indexes. Indexes are kept sorted by descending
    -- last usage order so we can quickly remove indexes which haven't been used
    -- for a long time.
    , iiLRCFirst  :: !(TVar (Maybe UserIndex)) -- ^ Most recent call.
    , iiLRCLast   :: !(TVar (Maybe UserIndex)) -- ^ Least recent call.
    }

-- Each API user has its own index. Index are used to search for image by tag or
-- by image code.

type UserName = Text

data UserIndex = UserIndex {
      uiName    :: !UserName
    , uiImages  :: !(TVar (Map ImageCode IndexedImage))
    , uiRootTag :: !Tag
    , uiLRCTime :: !(TVar UTCTime)           -- ^ Last time called.
    , uiLRCPrev :: !(TVar (Maybe UserIndex)) -- ^ More recently called user.
    , uiLRCNext :: !(TVar (Maybe UserIndex)) -- ^ Less recently called user.
    }

-- Here we define two types of tags.
-- The RootTag is used in the UserIndex to indicate the "catch all" tag. This
-- tag doesn't have any parent and has no name (it catches the empty tag
-- string). It contains only images which aren't registered to any tag.
-- The SubTag is used for "real" tags of the hierarchy.

newtype TagPath = TagPath { tpNodes :: [Text] }
    deriving (Eq, Ord, Read)

data TagType = RootTag | SubTag !Text !Tag -- ^ Tag\'s name and sub-tag.
    deriving (Eq, Ord)

data Tag = Tag {
      tType    :: !TagType
    , tSubTags :: !(TVar (Map Text Tag))
    -- | Only contains images which are not in sub-tags.
    , tImages  :: !(TVar (Set IndexedImage))
    }

instance Eq Tag where
    (==)    = (==)    `on` tType

instance Ord Tag where
    compare = compare `on` tType

-- | Conditional tag expression.
data TagExpression = TagName TagPath
                   | TagNot  TagExpression
                   | TagAnd  TagExpression TagExpression
                   | TagOr   TagExpression TagExpression

data IndexedImage = IndexedImage {
      iiCode :: !ImageCode
    , iiName :: !(Maybe Text)
    , iiTags :: !(Set Tag)
    , iiHist :: !IndexedHistogram
    } deriving (Eq, Ord)

type IndexedHistogram = HeterogeneousHistogram Float
