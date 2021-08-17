module Main where

import Codec.Picture

import Args (parseCmdArgs, masksPaths)
import Image ( LImage
              , getImage
              , width
              , height
              , maxFilter)
import Text.Printf (FieldFormat(fmtWidth))

main :: IO ()
main = do
    cdsOpts <- parseCmdArgs
    putStrLn (show cdsOpts)
    eimg <- getImage $ masksPaths cdsOpts
    case eimg of
        Left err -> putStrLn err
        Right img -> putStrLn (show (width fimg) ++ " " ++ show (height fimg))
            where
                fimg = maxFilter img 20
    return ()
