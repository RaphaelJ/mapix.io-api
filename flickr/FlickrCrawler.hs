{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Lazy as B
import Data.List (find)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getArgs)
import System.FilePath ((</>), (<.>), takeExtension)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Random
import Text.Printf

import Control.Monad.Logger (runNoLoggingT)

import Database.Persist (
      entityKey, entityVal, insert_, insertBy, insertUnique, selectList
    )
import Database.Persist.Sql (runMigration)
import Database.Persist.Sqlite (runSqlConn, withSqliteConn)

import qualified Flickr.API as F
import qualified Flickr.Photos as F

import Control.Monad.Trans.Resource (mkResource, runResourceT, with)
import Network.HTTP.Conduit (
      HttpException, Request, closeManager, conduitManagerSettings, httpLbs
    , managerResponseTimeout, newManager, parseUrl, responseBody
    )

import System.Random.Shuffle (shuffle')

import Model

data Photo = Photo {
      pId       :: Text
    , pTitle    :: Text
    , pUrl      :: Text
    , pTags     :: [Text]
    , pImageReq :: Request
    , pImageExt :: String
    , pWidth    :: Int
    , pHeight   :: Int
    } deriving Show

pageSize :: Int
pageSize = 10

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dictFile, sqliteFile, dstDir] -> do
            dict <- lines <$> readFile dictFile

            let httpSetts = conduitManagerSettings {
                  managerResponseTimeout = Just 30000
                }
                httpMan   = mkResource (newManager httpSetts) closeManager

            with httpMan $ \http ->
              withSqliteConn (T.pack sqliteFile) $ \conn -> do
                skipPics <- runSqlConn' conn $ do
                    runMigration migrateAll
                    selectList [] []

                let skipIds = map (T.unpack . flickrImagePhotoId. entityVal)
                                  skipPics

                pics <- liftIO $ photos (mkStdGen 0) dict skipIds

                forM_ (zip [(length skipIds + 1)..] pics) $ \(i, pic) -> do
                    mKey <- addPhoto http conn dstDir pic

                    case mKey of
                        Just key ->
                            liftIO $ printf "%d\t%s\t(key : %s)\n" i
                                            (T.unpack (pUrl pic)) (show key)
                        Nothing  -> return ()
        _            ->
            putStrLn "Usage: loader-flickr <dict file> <sqlite db> <dest dir>"
  where
    addPhoto http conn dstDir Photo { .. } = E.handle onException $ do
        let path = dstDir </> T.unpack pId <.> pImageExt

        bs <- httpLbs pImageReq http

        runSqlConn' conn $ runMaybeT $ do
            key <- MaybeT $ insertUnique $ FlickrImage pId pTitle pUrl pWidth
                                                       pHeight

            liftIO $ B.writeFile path (responseBody bs)

            lift $ mapM_ (insertTag key) pTags

            return key
      where
        onException (_ :: HttpException) = return Nothing

    insertTag picKey tag = do
        tagKey <- either entityKey id <$> insertBy (FlickrTag tag)
        insert_ $ FlickrImageTag picKey tagKey

    runSqlConn' conn action = 
        runNoLoggingT $ runResourceT $ runSqlConn action conn

-- | Returns an infinite list of FlickR photos taken from random searches using
-- the given dictionnary and random generator. Skips images from the given list.
photos :: RandomGen g => g -> [String] -> [F.PhotoID] -> IO [Photo]
photos gen dict skip =
    go dict' [] 1 (S.fromList skip)
  where
    filters = F.nullSearchConstraints {
          F.s_license = Just ["4"] -- Attribution License
        }

    dict' = shuffle' dict (length dict) gen

    go []       []  _    _   = return []
    go []       ws' page set = go (reverse ws') [] (page + 1) set
    go (w : ws) ws' page set = unsafeInterleaveIO $ do
        (ctx, pics) <- F.flick $ F.withPageSize pageSize $
            let filters' = filters { F.s_text = Just w }
            in F.pagedCall (Just page) $ F.search Nothing filters' []

        (res, set') <- goPix pics set

        let Just nPages = F.photoCtxtPages ctx
        rest <- if nPages > page then go ws (w : ws') page set'
                                 else go ws ws'       page set'

        return $ res ++ rest

    goPix []       set = return ([], set)
    goPix (p : ps) set | S.member picId set = goPix ps set
                       | otherwise          = E.handle onException $ do
        sizes <- F.flick $ F.getSizes picId
        let Just small = find ((== "Small 320") . F.sizeDetailsLabel) sizes
        imageRequest <- parseUrl $ F.sizeDetailsSource small

        details <- F.flick $ F.getInfo picId (Just secret)

        let photoId = T.pack picId
            title   = T.pack $ F.photoTitle p
            tags    = map (T.pack . F.tagDetailsName)
                          (F.photoDetailsTags details)
            urls    = F.photoDetailsURLs details
            url     = T.pack $ F.urlDetailsURL $ head urls
            ext     = takeExtension $ F.sizeDetailsSource small
            width   = F.sizeDetailsWidth  small
            height  = F.sizeDetailsHeight small
            photo   = Photo photoId title url tags imageRequest ext width height

        (rest, set') <- goPix ps (S.insert picId set)

        return (photo : rest, set')
      where
        picId  = F.photoId p
        secret = F.photoSecret p

        onException (_ :: E.SomeException) = goPix ps set