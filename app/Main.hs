module Main where

import Args ( parseCmdArgs
            , masksPaths
            , dataPaths
            , noMaskMirroring
            , shiftOption
            , maskThreshold
            , dataThreshold
            , pixColorFluctuation)
import BoxedImage ( BoxedImage )
import ImageProcessing (clearRegion)
import ColorDepthSearch ( ColorDepthQuery(..)
                        , ShiftOptions
                        , createAllColorDepthMasks
                        , calculateBestScore)

import qualified ImageIO as IIO(readImage)

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
                    let score = cds query (maskThreshold cdsOpts) (not (noMaskMirroring cdsOpts)) (shiftOption cdsOpts) target (dataThreshold cdsOpts) (pixColorFluctuation cdsOpts)
                    in print score
    return ()


readImageFromFile :: FilePath -> IO (Either String (BoxedImage Int))
readImageFromFile = IIO.readImage


cds :: BoxedImage Int -> Double -> Bool -> ShiftOptions -> BoxedImage Int -> Double -> Double -> Int
cds query queryThreshold mirror xyShift target targetThreshold pxFluctuation =
    let isLabelRegion = \x y -> x < 330 && y < 100 || x >= 950 && y < 85
        unlabeledQuery = clearRegion query isLabelRegion
        unlabeledTarget = clearRegion target isLabelRegion
        cdsMasks = createAllColorDepthMasks unlabeledQuery queryThreshold mirror xyShift
    in calculateBestScore cdsMasks unlabeledTarget targetThreshold pxFluctuation
