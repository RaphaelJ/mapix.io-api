{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Monad
import System.Directory (
      doesDirectoryExist, doesFileExist, getDirectoryContents
    )
import System.Environment (getArgs)
import System.FilePath (FilePath, (</>), takeFileName)
import Vision.Histogram (ToHistogram (..))
import qualified Vision.Histogram as H
import Vision.Image (Image (..), RGBImage)
import qualified Vision.Image as I

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dir] -> do
            dirs <- listDirs dir

            imgs <- concat <$> mapM loadSet dirs

            print ds
        _     -> putStrLn "Usage : ./train <training dir>"
  where
    -- Returns the list of non-hidden entries of a directory with an absolute
    -- path.
    listContent dir = map (dir </>) . excludeHidden <$> getDirectoryContents dir
    excludeHidden = filter (((/=) '.') . head)

    listDirs  dir = listContent dir >>= filterM doesDirectoryExist
    listFiles dir = listContent dir >>= filterM doesFileExist

    loadSet :: FilePath -> IO [(String, RGBImage)]
    loadSet dir = do
        let catName = takeFileName dir

        files <- listFiles dir
        forM files $ \file -> do
            Right img <- I.load file Nothing
            return (catName, I.convert img)

:: Image i, ToHistogram (ImagePixel i)
=> [(String, i)] -> (i -> Histogram sh a)
-> (Histogram sh a -> Histogram sh a -> b)
-> 
