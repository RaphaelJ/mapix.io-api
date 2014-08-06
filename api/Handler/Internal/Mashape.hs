-- | Utilities to read and write Mashape headers.
module Handler.Internal.Mashape (
      MashapeSubscription (..), MashapeHeaders (..)
    , getMashapeHeaders, maxIndexSize, setBilling
    ) where

import Import

import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

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
    MashapeHeaders <$> lookupHeader' "X-Mashape-Proxy-Secret"
                   <*> lookupHeader' "X-Mashape-User"
                   <*> lookupSubscription
                   <*> lookupHeader' "X-Forwarded-For"
  where
    lookupHeader' name = (decodeUtf8 . fromJust) <$> lookupHeader name

    lookupSubscription = do
            header <- lookupHeader' "X-Mashape-Subscription"
            case header of
                "FREE"    -> return MashapeFree
                "BASIC"   -> return MashapeBasic
                "PREMIUM" -> return MashapePremium
                "ULTRA"   -> return MashapeUltra
                "CUSTOM"  -> return MashapeCustom
                _         -> error "Invalid mashape plan."

maxIndexSize :: MashapeSubscription -> Maybe Int
maxIndexSize MashapeFree    = Just 500
maxIndexSize MashapeBasic   = Just 10000
maxIndexSize MashapePremium = Just 100000
maxIndexSize MashapeUltra   = Just 500000
maxIndexSize MashapeCustom  = Nothing

-- | Sets the X-Mashape-Billing header to the given list of values.
setBilling :: [(Text, Int)] -> Handler ()
setBilling xs =
    let val = T.intercalate ";" [ name <> (T.pack $ show n) | (name, n) <- xs ]
    in addHeader "X-Mashape-Billing" val
