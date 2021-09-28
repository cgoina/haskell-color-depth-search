{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Args ( parseCmdArgs
            , masksPaths
            , dataPaths
            , noMaskMirroring
            , shiftOption
            , maskThreshold
            , dataThreshold
            , pixColorFluctuation )
import Image1 ( Image, dims )
import Pixel ( RGB8Pixel )
-- import ImageProcessing (clearRegion)
-- import ColorDepthSearch ( ShiftOptions
--                         , calculateBestScore
--                         , createQueryMasks )
import qualified ImageIO1 as IIO(readImage)
import GHC.TypeNats (KnownNat, Nat)

main :: IO ()
main = do
    cdsOpts <- parseCmdArgs
    print cdsOpts
    qimg <- readImageFromFile $ masksPaths cdsOpts
    timg <- readImageFromFile $ dataPaths cdsOpts
    case qimg of
        Left err -> putStrLn err
        Right query ->
            case timg of
                Left err -> putStrLn err
                Right target ->
                    let d = dims query
                    in print d
    return ()


readImageFromFile :: FilePath
                  -> IO (Either String (Image w h RGB8Pixel))
readImageFromFile = IIO.readImage


-- cds :: BoxedImage Int -> Int -> Bool -> ShiftOptions -> BoxedImage Int -> Int -> Double -> Int
-- cds query queryThreshold mirror xyShift target targetThreshold pxFluctuation =
--     let isLabelRegion = \x y -> x < 330 && y < 100 || x >= 950 && y < 85
--         unlabeledQuery = clearRegion query isLabelRegion
--         unlabeledTarget = clearRegion target isLabelRegion
--         cdsMasks = createQueryMasks unlabeledQuery queryThreshold mirror xyShift
--     in calculateBestScore cdsMasks unlabeledTarget targetThreshold pxFluctuation
