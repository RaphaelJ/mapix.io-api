module ObjectIndex.Type where

import Prelude

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.STM (STM)
import Data.Function
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.String
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql (..))
import Yesod (PathPiece, ToJSON)

import Histogram (HeterogeneousHistogram)

-- | Runs the 'ObjectIndex' transaction inside a reader monad as some actions
-- need to known at what time the transaction was initiated.
type IndexSTM a = ReaderT UTCTime STM a

-- | 'ObjectCode's are unique randomly generated identifiers used to identify
-- objects.
--
-- An 'Object' is only unique for an user.
newtype ObjectCode = ObjectCode { ocValue :: Text }
    deriving (Eq, Ord, IsString, PersistField, PathPiece, ToJSON)

data ObjectIndex = ObjectIndex {
      oiUsers     :: !(TVar (Map UserName UserIndex))
    -- Here we define a concurrent double linked list which implements a Least
    -- Recently Used queue for user indexes. Indexes are kept sorted by
    -- descending last usage time so we can quickly remove indexes which haven't
    -- been used for a long time.
    , oiLRUFirst  :: !(TVar (Maybe UserIndex)) -- ^ Most recent call.
    , oiLRULast   :: !(TVar (Maybe UserIndex)) -- ^ Least recent call.
    }

-- Each API user has its own index. Index are used to search for objects by tag
-- or by object code.

type UserName = Text

data UserIndex = UserIndex {
      uiName    :: !UserName
    , uiObjects :: !(TVar (Map ObjectCode IndexedObject))
    , uiRootTag :: !Tag
    , uiLRUTime :: !(TVar UTCTime)           -- ^ Last time called.
    , uiLRUPrev :: !(TVar (Maybe UserIndex)) -- ^ More recently called user.
    , uiLRUNext :: !(TVar (Maybe UserIndex)) -- ^ Less recently called user.
    }

newtype TagPath = TagPath { tpNodes :: [Text] }
    deriving (Eq, Ord, Read, PersistField)

instance PersistFieldSql TagPath where
    sqlType = sqlType . (tpNodes `liftM`)

-- Here we define two kinds of tags.
-- The RootTag is used in the UserIndex to indicate the "catch all" tag. This
-- tag doesn't have any parent and has no name (it catches the empty tag
-- string). It contains only objects which aren't registered to any tag.
-- The SubTag is used for "real" tags of the hierarchy.

data TagType = RootTag | SubTag !Text !Tag -- ^ Tag\'s name and sub-tag.
    deriving (Eq, Ord)

data Tag = Tag {
      tType    :: !TagType
    , tSubTags :: !(TVar (Map Text Tag))
    -- | Only contains objects which are not in sub-tags.
    , tObjects :: !(TVar (Set IndexedObject))
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

data IndexedObject = IndexedObject {
      ioCode :: !ObjectCode
    , ioName :: !(Maybe Text)
    , ioTags :: !(Set Tag)
    , ioHist :: !IndexedHistogram
    } deriving (Eq, Ord)

type IndexedHistogram = HeterogeneousHistogram
