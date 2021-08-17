module Main where

import Args (parseCmdArgs, masksPaths)
import Image ( LImage
              , readImage
              , pixelAt
              , writeImageAsPng
              , width
              , height
              , pixels
              , maxFilter)
import Text.Printf (FieldFormat(fmtWidth))
import qualified Data.Vector         as V

main :: IO ()
main = do
    cdsOpts <- parseCmdArgs
    print cdsOpts
    eimg <- readImage $ masksPaths cdsOpts
    case eimg of
        Left err -> putStrLn err
        Right img ->
            -- print (
            --     show (length $ pixels img) 
            --  ++ " "
            --  ++ show (pixels img V.! 0)
            --  ++ " "
            --  ++ show (pixels img V.! 684859)
            -- )
            writeImageAsPng "tt.png" fimg
            where
                fimg = maxFilter img 20
    return ()
