module Histogram.Type where

import Prelude

import Data.Vector.Storable (Vector)
import Vision.Histogram (Histogram (..))
import Vision.Primitive (DIM1, DIM3)

type ColorIX = DIM3

type GreyIX = DIM1

data HeterogeneousHistogram a = HeterogeneousHistogram {
      hhColors    :: !(Histogram ColorIX a)
    , hhGreys     :: !(Histogram GreyIX  a)
    , hhColorsSum :: !a                      -- ^ @== sum hhColors@.
    } deriving (Eq, Ord, Show)

-- | Histogram on which some bins are undefined.
--
-- When compared to an 'HeterogeneousHistogram', any undefined bin will be
-- ignored in the computation of the matching score.
data PartialHistogram a = PartialHistogram {
      phColorsIxs    :: !(Vector Int) -- ^ Linear indexes of color bins.
    , phColorsValues :: !(Vector a)
    , phGreysIxs     :: !(Vector Int) -- ^ Linear indexes of grey-scale bins.
    , phGreysValues  :: !(Vector a)
    -- | @phSumValues == sum phColorsValues + sum phGreysValues@s 
    , phSumValues    :: !a
    } deriving (Show)
