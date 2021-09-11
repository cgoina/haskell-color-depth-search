module Main where

import Args (parseCmdArgs, masksPaths, maxFilterRadius)
import ArrayImage ( ArrayImage
                  , readArrayImage
                  , writeArrayImageAsPng
                  , clearRegion
                  , horizontalMirror
                  , maxFilter
                  )

main :: IO ()
main = do
    cdsOpts <- parseCmdArgs
    print cdsOpts
    eimg <- readArrayImage $ masksPaths cdsOpts
    case eimg of
        Left err -> putStrLn err
        Right img ->
            writeArrayImageAsPng "tt.png" fimg
            where
                fimg = maxFilter (maxFilterRadius cdsOpts) $ horizontalMirror $ clearRegion img isLabelRegion
                isLabelRegion = \x y -> x < 330 && y < 100 || x >= 950 && y < 85
    return ()
