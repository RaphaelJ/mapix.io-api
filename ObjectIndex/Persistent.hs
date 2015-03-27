-- | Provides primitives to manage the in-database index.
--
-- As some names conflict with those from ObjectIndex, the module is not
-- re-exported by ObjectIndex and need to be imported separately.
module ObjectIndex.Persistent (
      module ObjectIndex.Persistent.Manage
    , module ObjectIndex.Persistent.Model
    , module ObjectIndex.Persistent.Restore
    ) where

import ObjectIndex.Persistent.Manage
import ObjectIndex.Persistent.Model
import ObjectIndex.Persistent.Restore
