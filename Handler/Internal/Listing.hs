-- | Utility to create paginated listings of objects.
module Handler.Internal.Listing (
      Listing, ListingForm (..), listing, listingForm
    ) where

import ClassyPrelude

import Yesod hiding (count)

import Handler.Config (confDefaultCount, confMaxCount)

data Listing a = Listing {
      lTotalCount :: Int
    , lOffset     :: Int
    , lCount      :: Int
    , lObjects    :: [a]
    }

instance ToJSON a => ToJSON (Listing a) where
    toJSON Listing {..} = object [ "total_count" .= lTotalCount
                                 , "offset"      .= lOffset
                                 , "count"       .= lCount
                                 , "results"     .= lObjects ]

data ListingForm = ListingForm {
      lfOffset   :: Maybe Int
    , lfCount    :: Maybe Int
    }

-- | @listing form mLen xs@ will create a 'Listing' object from a subset of
-- @xs@.
listing :: ListingForm -> Maybe Int -> [a] -> Listing a
listing ListingForm {..} mLen xs =
    let len    = fromMaybe (length xs) mLen
        offset = min len (fromMaybe 0 lfOffset)
        count  = min (len - offset) (fromMaybe confDefaultCount lfCount)
        xs'    = take count $ drop offset xs
    in Listing len offset count xs'

listingForm :: (RenderMessage (HandlerSite m) FormMessage, Monad m)
            => FormInput m ListingForm
listingForm = ListingForm <$> iopt positiveField "offset"
                          <*> iopt countField    "count"

countField :: (RenderMessage (HandlerSite m) FormMessage, Monad m)
           => Field m Int
countField =
    let msgMax = asText $! pack $!    "The value can't be higher than "
                                   ++ show confMaxCount
        msgMin = asText "The value must positive or zero"
    in checkBool (<= confMaxCount) msgMax $
       checkBool (>= 0)            msgMin intField

positiveField :: (RenderMessage (HandlerSite m) FormMessage, Monad m)
              => Field m Int
positiveField = checkBool (>= 0) ("Negative value not allowed" :: Text) intField
