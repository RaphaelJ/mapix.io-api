module Util.Mashape (
      MashapeSubscription (..), MashapeHeaders (..)
    ) where

import Import

import Control.Applicative

data MashapeSubscription = MashapeFree  | MashapeBasic | MashapePremium
                         | MashapeUltra | MashapeCustom
    deriving Show

data MashapeHeaders = MashapeHeaders {
      mhProxySecret  :: Text -- ^ Secret key from the Mashape proxy.
    , mhUser         :: Text -- ^ Username of the client.
    , mhSubscription :: MashapeSubscription
    , mhForwardedFor :: Text -- ^ Host/Ip of the client.
    } deriving Show

getMashapeHeaders :: Handler MashapeHeaders
getMashapeHeaders =
    let lookupHeader' name = fromJust <$> lookupHeader name
        lookupSubscription = do
            header <- lookupHeader' "X-Mashape-Subscription"
            case header of
                "FREE"    -> MashapeFree
                "BASIC"   -> MashapeBasic
                "PREMIUM" -> MashapePremium
                "ULTRA"   -> MashapeUltra
                "CUSTOM"  -> MashapeCustom
                _         -> error "Invalid mashape plan."
    in MashapeHeaders <$> lookupHeader' "X-Mashape-Proxy-Secret"
                      <*> lookupHeader' "X-Mashape-User"
                      <*> lookupSubscription
                      <*> lookupHeader' "X-Forwarded-For"
