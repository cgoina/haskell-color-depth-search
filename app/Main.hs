module Main where

import Codec.Picture

import Args (parseCmdArgs, masksPaths)
import Image (getImage, makeLineRadii)

main :: IO ()
main = do
    cdsOpts <- parseCmdArgs
    putStrLn (show cdsOpts)
    eimg <- getImage $ masksPaths cdsOpts
    let rs = makeLineRadii 20.0
    putStrLn (show rs)
    case eimg of
        Left err -> putStrLn err
        Right img -> putStrLn (show (imageWidth img) ++ " " ++ show (imageHeight img))
    return ()
