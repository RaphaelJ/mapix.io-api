-- | This module contains anything related to the computation and conversion of
-- histograms.
module Histogram (
      module Histogram.Color
    , module Histogram.Compare
    , module Histogram.Compress
    , module Histogram.Compute
    , module Histogram.Config
    , module Histogram.Type
    ) where

import Histogram.Color
import Histogram.Compare
import Histogram.Compress
import Histogram.Compute
import Histogram.Config
import Histogram.Serialize ()
import Histogram.Type
