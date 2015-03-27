import Vision.Detector.Edge
import qualified Vision.Image as I
import Vision.Primitive
import Data.Ratio

import Control.Monad.ST
import Data.Int
import System.Environment

main = do
    [i, b, e, c, o, f, g, r, low, high] <- getArgs

    Right io <- I.load i Nothing

    let rgb :: I.RGBImage
        rgb = resize' $ I.convert io

        grey, blurred, edges, flooded :: I.GreyImage
        !grey = I.convert rgb
        gaussianBlur :: I.SeparableFilter I.GreyPixel Float I.GreyPixel
        !gaussianBlur = I.gaussianBlur (read g) Nothing
        !blurred = I.apply grey gaussianBlur
        !edges = canny (read r) (read low) (read high) blurred
        !closed = closure 2 edges

        !flooded = I.create $ do
                mEdges <- I.thaw closed :: ST s (I.MutableManifest I.GreyPixel s)
                I.floodFill (ix2 0     0)     mEdges 127
                I.floodFill (ix2 (h-1) 0)     mEdges 127
                I.floodFill (ix2 (h-1) (w-1)) mEdges 127
                I.floodFill (ix2 0     (w-1)) mEdges 127
                return mEdges

        final :: I.RGBAImage
        !final = I.fromFunction size $ \pt ->
            if flooded `I.index` pt == 127
               then I.RGBAPixel 0 0 0 0
               else let I.RGBPixel r g b = rgb `I.index` pt
                    in I.RGBAPixel r g b 255

    I.save b blurred
    I.save e edges
    I.save c closed
    I.save o flooded
    I.save f final
  where
    resize' :: I.RGBImage -> I.RGBImage
    resize' !img =
        let Z :. y :. x = I.shape img
            ratio = max y x % 320
            size' = ix2 (round $ fromIntegral y / ratio)
                        (round $ fromIntegral x / ratio)
        in I.resize img I.Bilinear size'

    openning, closure :: Int -> I.GreyImage -> I.GreyImage
    openning r i =
        let i' :: I.GreyImage
            i' = i `I.apply` I.erode r
        in i' `I.apply` I.dilate r

    closure r i =
        let i' :: I.GreyImage
            i' = i `I.apply` I.dilate r
        in i' `I.apply` I.erode r

square :: Num a => a -> a
square a = a * a