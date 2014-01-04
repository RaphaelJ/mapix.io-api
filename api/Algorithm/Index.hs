module Algorithm.Index (ImageIndex)
    where

type UserName = Text

newtype ImageIndex = ImageIndex (Map UserName UserIndex)
    deriving (Show)
    
data UserIndex = UserIndex {
      uiTags    :: Map TagName UserIndex
    , uiImages  :: [Image]
    }

data Image = Image {
      iHist     :: !Histogram
    }

data Histogram = Histogram {
      h5D       :: !(H.Histogram DIM5 Float)
    , h3D       :: !(H.Histogram DIM5 Float)
    , hCount    :: !(TVar Int)
    }