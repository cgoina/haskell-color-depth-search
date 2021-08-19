module Main where

import Args (parseCmdArgs, masksPaths)
import Image ( LImage
              , readImage
              , writeImageAsPng
              , maxFilter
              , horizontalMirror
              , clearRegion)

main :: IO ()
main = do
    cdsOpts <- parseCmdArgs
    print cdsOpts
    eimg <- readImage $ masksPaths cdsOpts
    case eimg of
        Left err -> putStrLn err
        Right img ->
            writeImageAsPng "tt.png" filteredImg
            where
                filteredImg = maxFilter 10 (clearRegion img isLabelRegion)
                isLabelRegion = \x y -> x < 330 && y < 100 || x >= 950 && y < 85
    return ()
