module Histogram.Type where

import ClassyPrelude

import Vision.Histogram (Histogram (..))
import Vision.Image.Storage.DevIL (StorageImage)
import Vision.Primitive (DIM1, DIM3)

-- | Image which has been resized for its histogram to be computed.
newtype ResizedImage = ResizedImage { riImage :: StorageImage }

type Weight = Float

-- -- | Defines the similarity between two bins in a cross bin comparison.
-- data BinsSimilarity = BinsSimilarity {
--     -- | First bin index.
--       bsBin1 :: Int
--     -- | Second bin index.
--     , bsBin2 :: Int
--     -- | Similarity factor for this pair of bins.
--     -- Defines the weight of the comparison with the corresponding bin.
--     -- Direct bin comparison is weighted at 1.
--     -- e.g. 0.25 defines that the comparison with the previous bin has one
--     -- fourth of the weight of a direct bin comparison.
--     , bsSimilarity :: Weight
--     } deriving Show

type ColorIX = DIM3

type GreyIX = DIM1

data HeterogeneousHistogram = HeterogeneousHistogram {
      hhColors    :: !(Histogram ColorIX Weight)
    , hhGreys     :: !(Histogram GreyIX  Weight)
    , hhColorsSum :: !Weight                     -- ^ @== sum hhColors@.
    } deriving (Eq, Ord, Show)

-- | Utility to construct an 'HeterogeneousHistogram' without having to compute
-- 'hhColorsSum'.
heterogeneousHistogram :: Histogram ColorIX Weight -> Histogram GreyIX  Weight
                       -> HeterogeneousHistogram
heterogeneousHistogram colors greys =
    HeterogeneousHistogram colors greys (sum $ vector colors)

-- | Histogram on which some bins are undefined.
--
-- When compared to an 'HeterogeneousHistogram', any undefined bin will be
-- ignored in the computation of the matching score.
data PartialHistogram = PartialHistogram {
      phColorsIxs    :: !(SVector Int) -- ^ Linear indexes of color bins.
    , phColorsValues :: !(SVector Weight)
    , phGreysIxs     :: !(SVector Int) -- ^ Linear indexes of grey-scale bins.
    , phGreysValues  :: !(SVector Weight)
    -- | @phSumValues == sum phColorsValues + sum phGreysValues@s 
    , phSumValues    :: !Weight
    } deriving Show
