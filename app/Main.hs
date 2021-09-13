module Main where

import Args ( parseCmdArgs
            , masksPaths
            , dataPaths
            , noMaskMirroring
            , maskThreshold
            , dataThreshold
            , pixColorFluctuation)
import BoxedImage ( BoxedImage )
import ImageProcessing (clearRegion)
import ColorDepthSearch ( ColorDepthQuery(..)
                        , createAllColorDepthQueries
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
                    let score = cds query (maskThreshold cdsOpts) (not (noMaskMirroring cdsOpts)) target (dataThreshold cdsOpts) (pixColorFluctuation cdsOpts)
                    in print score
    return ()


readImageFromFile :: FilePath -> IO (Either String (BoxedImage Int))
readImageFromFile = IIO.readImage


cds :: BoxedImage Int -> Double -> Bool -> BoxedImage Int -> Double -> Double -> Int
cds query queryThreshold mirror target targetThreshold pxFluctuation =
    let isLabelRegion = \x y -> x < 330 && y < 100 || x >= 950 && y < 85
        unlabeledQuery = clearRegion query isLabelRegion
        unlabeledTarget = clearRegion target isLabelRegion
        cdsQueries = createAllColorDepthQueries unlabeledQuery queryThreshold targetThreshold pxFluctuation mirror
    in calculateBestScore cdsQueries unlabeledTarget

