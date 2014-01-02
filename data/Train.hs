{-# LANGUAGE BangPatterns, FlexibleContexts, RecordWildCards, TypeFamilies
  , TypeOperators #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Parallel.Strategies
import Data.Function
import Data.Int
import Data.List
import Data.Ratio
import Data.Word
import System.Directory (
      doesDirectoryExist, doesFileExist, getDirectoryContents
    )
import System.Environment (getArgs)
import System.FilePath ((</>), takeFileName)
import Text.Printf
import Vision.Histogram (Histogram, ToHistogram (..))
import qualified Vision.Histogram as H
import Vision.Image (Image (..), HSVImage, HSVPixel (..), RGBImage)
import qualified Vision.Image as I
import Vision.Primitive

maxImageSize :: Int
maxImageSize = 320

data Tag = Tag {
      tagFilename :: !FilePath
    , tagCategory :: !String
    }

data Config sh norm score = Config {
      confHistSize :: sh
    , confHistNorm :: Histogram sh Double -> Histogram sh norm
    , confCompare  :: Histogram sh norm -> Histogram sh norm -> score
    -- | How scores are sorted. Best scores must be sorted first (LT).
    , confSortBy   :: score -> score -> Ordering
    }

configCorrel :: Shape sh => sh -> Config sh Float Double
configCorrel histSize = Config {
      confHistSize = histSize
    , confHistNorm = H.map realToFrac
    , confCompare  = \hist1 hist2 -> H.compareCorrel hist1 hist2 :: Double
    , confSortBy   = reverseCompare
    }

configChi :: Shape sh => sh -> Config sh Float Double
configChi histSize = Config {
      confHistSize = histSize
    , confHistNorm = H.map realToFrac
    , confCompare  = \hist1 hist2 -> H.compareChi hist1 hist2 :: Double
    , confSortBy   = compare
    }

configChiWord8 :: Shape sh => sh -> Config sh Word8 Double
configChiWord8 histSize = Config {
      confHistSize = histSize
    , confHistNorm = H.map (round . (* 255))
    , confCompare  = \hist1 hist2 -> H.compareChi hist1 hist2 :: Double
    , confSortBy   = compare
    }

configIntersec :: Shape sh => sh -> Config sh Float Double
configIntersec histSize = Config {
      confHistSize = histSize
    , confHistNorm = H.map realToFrac
    , confCompare  = \hist1 hist2 -> realToFrac (H.compareIntersect hist1 hist2)
    , confSortBy   = reverseCompare
    }

configIntersecWord8 :: Shape sh => sh -> Config sh Word8 Int
configIntersecWord8 histSize = Config {
      confHistSize = histSize
    , confHistNorm = H.map (round . (* 255))
    , confCompare  = \hist1 hist2 -> int (H.compareIntersect hist1 hist2)
    , confSortBy   = reverseCompare
    }

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dir] -> do
            dirs <- listDirs dir

            rgbs <- concat <$> mapM loadSet dirs

            let hsvs      = map (second ((shiftHue 8) . toHSV)) rgbs
                hist3D    = ix5 8 10 7 1 1
                hist5D    = ix5 8 10 7 3 3
                hsvHist3D = ix5 8 4 4 1 1
                hsvHist5D = ix5 8 4 4 3 3
                strat     = parList $
                    evalTuple3 rseq rseq $
                        evalList $ evalTuple2 rseq rseq
                results = [
                      ("RGB 3D X²", "green"
                      , testParams rgbs (configChi hist3D))

                    , ("RGB 5D X²", "magenta"
                      , testParams rgbs (configChi hist5D))

                    , ("HSV 3D X²", "blue"
                      , testParams hsvs (configChi hsvHist3D))

                    , ("HSV 5D X²", "red"
                      , testParams hsvs (configChi hsvHist5D))

                    , ("HSV 3D intersec", "grey"
                      , testParams hsvs (configIntersec hsvHist3D))

                    , ("HSV 5D intersec", "black"
                      , testParams hsvs (configIntersec hsvHist5D))
                    ] `using` strat

            printMatches (bestMatches hsvs (configChi hsvHist5D) 10)

--             plotResults results 0.1
        _     -> putStrLn "Usage : ./train <training dir>"
  where
    -- Returns the list of non-hidden entries of a directory with an absolute
    -- path.
    listContent dir = map (dir </>) . excludeHidden <$> getDirectoryContents dir
    excludeHidden = filter (((/=) '.') . head)

    listDirs  dir = listContent dir >>= filterM doesDirectoryExist
    listFiles dir = listContent dir >>= filterM doesFileExist

    loadSet :: FilePath -> IO [(Tag, RGBImage)]
    loadSet dir = do
        let catName = takeFileName dir

        files <- listFiles dir
        forM files $ \filename -> do
            Right img <- I.load filename Nothing
            let rgb   = I.convert img :: RGBImage
            return (Tag filename catName, resize rgb)

    resize img | maxSide > maxImageSize =
                    let ratio = maxSide % maxImageSize
                        w'    = round $ w % 1 * ratio
                        h'    = round $ h % 1 * ratio
                    in I.resize img I.Bilinear (Z :. h' :. w')
               | otherwise              = img
      where
        Z :. h :. w = I.shape img
        maxSide = max w h

    toHSV img = I.convert img :: HSVImage

    shiftHue :: Int -> HSVImage -> HSVImage
    shiftHue nColors =
        let !padd = 180 `quot` (nColors * 2)
        in I.map (\pix@(HSVPixel {..}) ->
                pix { hsvHue = word8 ((int hsvHue + padd) `mod` 180) }) 

    printMatches matches = do
        putStrLn "<html>"
        putStrLn "<body>"

        forM_ matches $ \(tested, matched) -> do
            putStrLn "<p>"
            _ <- printf "<img src='%s'/><br />" (tagFilename tested)
            putStrLn "<table>"
            putStrLn "<tr>"
            forM_ matched $ \(match, _) ->
                printf "<td><img src='%s'/><td>" (tagFilename match)
            putStrLn "</tr>"
            putStrLn "<tr>"
            forM_ matched $ \(_, score) ->
                printf "<td>%f<td>" (realToFrac score :: Double)
            putStrLn "</tr>"
            putStrLn "</table>"
            putStrLn "</p>"

        putStrLn "</body>"
        putStrLn "</html>"

    plotResults configs maxNegRate = do
        putStrLn "set xlabel 'False positives rate'"
        putStrLn "set ylabel 'Retrived rate'"

        putStr "plot "
        forM_ configs $ \(confName, confColor, _) -> do
            printf "'-' with lines title '%s' lt rgb '%s'," confName confColor
        putStrLn ""

        forM_ configs $ \(_, _, scores) -> do
            let applyMaxNegRate = takeWhile ((<= maxNegRate) . snd) scores
            forM_ applyMaxNegRate $ \(posRate, negRate) -> do
                printf "%f %f\n" negRate posRate
            putStrLn "e"

calcHists :: (Image i, ToHistogram (ImagePixel i)
             , sh ~ PixelValueSpace (ImagePixel i), H.HistogramShape sh)
          => [(Tag, i)] -> Config (sh :. Int :. Int) norm score
          -> [(Tag, Histogram (sh :. Int :. Int) norm)]
calcHists imgs Config { .. } =
    let calcHist img =
            confHistNorm $ normalize' $ H.histogram2D img confHistSize
        normalize' :: Histogram sh Int32 -> Histogram sh Double
        normalize' hist = H.normalize hist 1.0
    in map (second calcHist) imgs

bestMatches :: (Image i, ToHistogram (ImagePixel i)
               , sh ~ PixelValueSpace (ImagePixel i), H.HistogramShape sh)
            => [(Tag, i)] -> Config (sh :. Int :. Int) norm score
            -> Int                     -- ^ Number of matches per image
            -> [(Tag, [(Tag, score)])] -- ^ (tested image, best matches)
bestMatches imgs conf@(Config { .. }) n =
    let hists = calcHists imgs conf
    in go [] hists
  where
    -- Compares every histogram with every other histogram.
    -- Returns for each histogram the best matches.
    -- positive test or not.
    go _   []                           = []
    go acc (x@(name, fingerprint) : xs) =
        let matches = findMatches fingerprint (acc ++ xs)
        in (name, matches) : go (x : acc) xs

    findMatches fingerprint xs =
        let scores = map (compareWith fingerprint) xs
        in take n $ sortBy (confSortBy `on` snd) scores

    compareWith fingerprint (name, fingerprint') =
        (name, confCompare fingerprint fingerprint')

testParams :: (Image i, ToHistogram (ImagePixel i), Eq score
              , sh ~ PixelValueSpace (ImagePixel i), H.HistogramShape sh)
           => [(Tag, i)] -> Config (sh :. Int :. Int) norm score
           -> [(Double, Double)] -- ^ (retreive rate, false positives rate)
testParams imgs conf@(Config { .. }) =
    let hists = calcHists imgs conf
        scores = sortBy (confSortBy `on` snd) (goCompare [] hists)
        (nPos, nNeg) = (length *** length) $ partition fst scores
    in goScores 0.0 0.0 nPos nNeg scores
  where
    -- Compares every histogram with every other histogram.
    -- Returns the list of scores and a boolean which indicates if it was a
    -- positive test or not.
    goCompare _   []       = []
    goCompare acc (x : xs) =
        let scores = map (compareTwo x) (acc ++ xs)
        in scores ++ goCompare (x : acc) xs

    goScores posRate negRate _    _    []                  =
        [(posRate, negRate)]
    goScores posRate negRate nPos nNeg scores@((_, s) : _) =
        let (sameScore, lowerScore) = span ((s ==) . snd) scores
            (nVal, nInval) = (length *** length) $ partition fst sameScore
            posRate' = posRate + (double nVal)   / (double nPos)
            negRate' = negRate + (double nInval) / (double nNeg)
        in (posRate, negRate) : goScores posRate' negRate' nPos nNeg lowerScore

    compareTwo (tag, fingerprint) (tag', fingerprint') =
        ( tagCategory tag == tagCategory tag'
        , confCompare fingerprint fingerprint')

reverseCompare :: Ord a => a -> a -> Ordering
reverseCompare a b =
    case compare a b of
        LT -> GT
        GT -> LT
        EQ -> EQ

double :: Integral a => a -> Double
double = fromIntegral
int :: Integral a => a -> Int
int = fromIntegral
word8 :: Integral a => a -> Word8
word8 = fromIntegral
