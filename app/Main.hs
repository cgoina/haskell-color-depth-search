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
import Image1 ( Image )
import Pixel ( Pixel(clear), RGB8Pixel )
import ImageProcessing1 ( clearRegion, horizontalMirror )
-- import ColorDepthSearch ( ShiftOptions
--                         , calculateBestScore
--                         , createQueryMasks )
import qualified ImageIO1 as IIO(readImage, writeImageAsPng)
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
                Right target -> do
                    IIO.writeImageAsPng "ttq.png" query
                    IIO.writeImageAsPng "ttt.png" (horizontalMirror (clearRegion query isLabelRegion))
    return ()


readImageFromFile :: FilePath -> IO (Either String (Image w h RGB8Pixel))
readImageFromFile = IIO.readImage


-- cds :: BoxedImage Int -> Int -> Bool -> ShiftOptions -> BoxedImage Int -> Int -> Double -> Int
-- cds query queryThreshold mirror xyShift target targetThreshold pxFluctuation =
--     let isLabelRegion = \x y -> x < 330 && y < 100 || x >= 950 && y < 85
--         unlabeledQuery = clearRegion query isLabelRegion
--         unlabeledTarget = clearRegion target isLabelRegion
--         cdsMasks = createQueryMasks unlabeledQuery queryThreshold mirror xyShift
--     in calculateBestScore cdsMasks unlabeledTarget targetThreshold pxFluctuation


isLabelRegion :: Int -> Int -> Bool
isLabelRegion x y = x < 330 && y < 100 || x >= 950 && y < 85
