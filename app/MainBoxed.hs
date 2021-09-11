module MainBoxed where

import Args (parseCmdArgs, masksPaths, maxFilterRadius)
import BoxedImage ( BoxedImage
                  , readBoxedImage
                  , writeBoxedImageAsPng
                  , horizontalMirror
                  , clearRegion
                  , maxFilter
                  )

main :: IO ()
main = do
    cdsOpts <- parseCmdArgs
    print cdsOpts
    eimg <- readBoxedImage $ masksPaths cdsOpts
    case eimg of
        Left err -> putStrLn err
        Right img ->
            writeBoxedImageAsPng "tt.png" fimg
            where
                fimg = maxFilter (maxFilterRadius cdsOpts) $ clearRegion img isLabelRegion
                isLabelRegion = \x y -> x < 330 && y < 100 || x >= 950 && y < 85
    return ()
