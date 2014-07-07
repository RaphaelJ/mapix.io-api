module Util.Mashape (
      MashapeSubscription (..), MashapeHeaders (..)
    , getMashapeHeaders, maxIndexSize
    ) where

import Import

import Control.Applicative
import qualified Data.Text as T

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

maxIndexSize :: MashapeSubscription -> Maybe Int
maxIndexSize MashapeFree    = Just 500
maxIndexSize MashapeBasic   = Just 10000
maxIndexSize MashapePremium = Just 100000
maxIndexSize MashapeUltra   = Just 500000
maxIndexSize MashapeCustom  = Nothing

-- | Sets the X-Mashape-Billing header to the given list of values.
setBilling :: [(Text, Int)] -> Handler ()
setBilling xs =
    let val = T.intercalate ";" [ name <> "| (name, n) <- xs 
    in addHeader "X-Mashape-Billing" val
