module Histogram.Type where

import Prelude

import Data.Vector.Storable (Vector)
import Vision.Histogram (Histogram (..))
import Vision.Primitive (DIM1, DIM3)

import qualified Data.Vector.Generic            as G
import qualified Data.Vector.Generic.Mutable    as M

type Weight = Float

-- | Defines the similarity between two bins in a cross bin comparison.
data BinsSimilarity = BinsSimilarity {
    -- | First bin index.
    , bsBin1 :: Int
    -- | Second bin index.
    , bsBin2 :: Int
    -- | Similarity factor for this pair of bins.
    -- Defines the weight of the comparison with the corresponding bin.
    -- Direct bin comparison is weighted at 1.
    -- e.g. 0.25 defines that the comparison with the previous bin has one
    -- fourth of the weight of a direct bin comparison.
    , bsSimilarity :: Weight
    } deriving Show

type ColorIX = DIM3

type GreyIX = DIM1

data HeterogeneousHistogram = HeterogeneousHistogram {
      hhColors    :: !(Histogram ColorIX Weight)
    , hhGreys     :: !(Histogram GreyIX  Weight)
    , hhColorsSum :: !Weight                     -- ^ @== sum hhColors@.
    } deriving (Eq, Ord, Show)

-- | Histogram on which some bins are undefined.
--
-- When compared to an 'HeterogeneousHistogram', any undefined bin will be
-- ignored in the computation of the matching score.
data PartialHistogram = PartialHistogram {
      phColorsIxs    :: !(Vector Int) -- ^ Linear indexes of color bins.
    , phColorsValues :: !(Vector Weight)
    , phGreysIxs     :: !(Vector Int) -- ^ Linear indexes of grey-scale bins.
    , phGreysValues  :: !(Vector Weight)
    -- | @phSumValues == sum phColorsValues + sum phGreysValues@s 
    , phSumValues    :: !Weight
    } deriving Show
