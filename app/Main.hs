module Main where

import Args (parseCmdArgs, masksPaths, maxFilterRadius)
import Image ( Image
              , readImage
              , writeImageAsPng
              , horizontalMirror
              , clearRegion
              , maxFilter
              )

main :: IO ()
main = do
    cdsOpts <- parseCmdArgs
    print cdsOpts
    eimg <- readImage $ masksPaths cdsOpts
    case eimg of
        Left err -> putStrLn err
        Right img ->
            writeImageAsPng "tt.png" fimg
            where
                fimg = maxFilter (maxFilterRadius cdsOpts) $ clearRegion img isLabelRegion
                isLabelRegion = \x y -> x < 330 && y < 100 || x >= 950 && y < 85
    return ()
