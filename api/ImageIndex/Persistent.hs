-- | Provides primitives to manage the in-database index.
--
-- As some names conflict with those from ImageIndex, the module is not
-- re-exported by ImageIndex and need to be imported separately.
module ImageIndex.Persistent (
      module ImageIndex.Persistent.Manage
    , module ImageIndex.Persistent.Model
    , module ImageIndex.Persistent.Restore
    ) where

import ImageIndex.Persistent.Manage
import ImageIndex.Persistent.Model
import ImageIndex.Persistent.Restore
