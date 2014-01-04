module Algorithm.Index (ImageIndex)
    where

type UserName = Text

type HistogramHash = ByteString

data ImageIndex = ImageIndex {
      iiUsers   :: !(TVar (Map UserName      UserIndex))
    , iiHists   :: !(TVar (Map HistogramHash Histogram))
    } deriving Show

type TagName = Text

data UserIndex = UserIndex {
      uiTags    :: !(TVar (Map TagName UserIndex))
    , uiImages  :: !(TVar (Map Hmac    Image))
    } deriving Show

data Image = Image {
      iHmac     :: !Hmac
    , iName     :: !(Maybe Text)
    , iHist     :: !Histogram
    } deriving (Show, Ord)

data Histogram = Histogram {
      h5D       :: !(H.Histogram DIM5 Float)
    , h3D       :: !(H.Histogram DIM5 Float)
    , hCount    :: !(TVar Int)
    } deriving Show
