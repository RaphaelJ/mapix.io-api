import Criterion.Main
import Vision.Image (StorageImage)
import qualified Vision.Image as I

import Histogram (fromImage, resize, backgroundMask)
import ImageIndex (IndexedHistogram)

path :: FilePath
path = "bench/image.jpg"

main :: IO ()
main = do
    Right io <- I.load Nothing path

    let resized = resize io

    defaultMain [
          bgroup "Histogram.Compute" [
              bench "fromImage (without background extraction)" $
                whnf (fromImage' False False) resized
            , bench "fromImage (with background extraction)" $
                whnf (fromImage' True False)  resized
            , bench "resize"         $ whnf resize         io
            , bench "backgroundMask" $ whnf backgroundMask resized
            ]
        ]
  where
    fromImage' :: Bool -> Bool -> StorageImage -> IndexedHistogram
    fromImage' = fromImage
