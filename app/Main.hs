module Main where

import qualified Codec.Picture as JP

import Args (parseCmdArgs, masksPaths, maxFilterRadius)
import BoxedImage ( BoxedImage )
import ImageIO (readImage, writeImageAsPng)
import ImageProcessing (maxFilter, clearRegion, horizontalMirror)


import qualified ImageIO as IIO(readImage)

main :: IO ()
main = do
    cdsOpts <- parseCmdArgs
    print cdsOpts
    eimg <- readImageFromFile $ masksPaths cdsOpts
    case eimg of
        Left err -> putStrLn err
        Right img ->
            writeImageAsPng "tt.png" fimg
            where
                fimg = maxFilter (maxFilterRadius cdsOpts) $ horizontalMirror $ clearRegion img isLabelRegion
                isLabelRegion = \x y -> x < 330 && y < 100 || x >= 950 && y < 85
    return ()


readImageFromFile :: FilePath -> IO (Either String (BoxedImage Int))
readImageFromFile = IIO.readImage
