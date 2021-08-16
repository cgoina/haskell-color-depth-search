module Main where

import Codec.Picture

import Args (parseCmdArgs, masksPaths)
import Image (LImage,
              width,
              height,
              getImage,
              makeLineRadii)

main :: IO ()
main = do
    cdsOpts <- parseCmdArgs
    putStrLn (show cdsOpts)
    eimg <- getImage $ masksPaths cdsOpts
    let rs = makeLineRadii 20.0
    putStrLn (show rs)
    case eimg of
        Left err -> putStrLn err
        Right img -> putStrLn (show (width img) ++ " " ++ show (height img))
    return ()
