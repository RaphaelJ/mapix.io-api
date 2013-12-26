import Control.Applicative
import Control.Monad
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath (FilePath, (</>))
import qualified Vision.Histogram as H
import qualified Vision.Image as I

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dir] -> do
            dirs <- catsDirs dir
            mapM loadDir dirs

            

            print ds
        _     -> putStrLn "Usage : ./train <training dir>"
  where
    -- Returns every dirs in the training directory.
    catsDirs dir =
        map (dir </>) . excludeHidden <$> getDirectoryContents dir >>=
        filterM doesDirectoryExist

    excludeHidden = filter (((/=) '.') . head)