{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

module ColorDepthSearch where

import Data.Word ( Word8 )

import Image( Image( getAt
                   , width)
            , Pixel(rgb)
            , regionPixelsAndCoords
            , aboveThreshold )
import ImageProcessing ( horizontalMirror )


data ShiftOptions = None | One | Two
                    deriving Show


data ColorDepthQuery t z s p = ColorDepthQuery {
    queryThreshold :: (Ord t, Num t) => t
  , targetThreshold :: (Ord t, Num t) => t -- target threshold
  , zTolerance :: RealFrac z => z -- pixel color fluctuation
  , queryImage :: Image s p => s p -- query image
}


getXyShift :: ShiftOptions -> Int
getXyShift None = 0
getXyShift One = 1
getXyShift Two = 2


createAllColorDepthMasks :: (Image s p, Ord t, Num t)
    => s p -- query image
    -> t -- mask threshold
    -> Bool -- mirror the mask
    -> ShiftOptions -- shift options
    -> [[(Int,p)]]
createAllColorDepthMasks qImg maskThreshold mirrorFlag pixelShift =
    let w = width qImg
        xyShift = getXyShift pixelShift
        xyShiftTransforms = [\(x,y) -> (x+dx,y+dy) | dy <- [-xyShift..xyShift], dx <- [-xyShift..xyShift]]
        xyShiftMirrorTransforms = [\(x,y) -> (w-(x+dx)-1,y+dy) | dy <- [-xyShift..xyShift], dx <- [-xyShift..xyShift]]
        masksExtractor = map (flip (regionPixelsAndCoords qImg) (`aboveThreshold` maskThreshold))
        masks = masksExtractor xyShiftTransforms
        mirrorMasks = if mirrorFlag then masksExtractor xyShiftMirrorTransforms else []
    in
        masks ++ mirrorMasks


calculateBestScore :: (Num t, Ord t, RealFrac z, Image s p) => [[(Int,p)]] -> s p -> t -> z -> Int
calculateBestScore queries target targetThreshold pixColorFluctuation = 
    let calcMaskScore = \m -> calculateScore m target targetThreshold pixColorFluctuation
    in maximum $ map calcMaskScore queries


calculateScore :: (Num t, Ord t, RealFrac z, Image s p) => [(Int, p)] -> s p -> t -> z -> Int
calculateScore mask targetImage targetThreshold pixColorFluctuation =
    let queryPixelPos = map fst mask 
        queryPixels = map snd mask
        targetPixels = map (getAt targetImage) queryPixelPos
        pixelsToCompare = filter ((`aboveThreshold` targetThreshold) . snd) $ zip queryPixels targetPixels
        pxGaps = map (uncurry pixelGap) pixelsToCompare

    in length $ filter (pixColorFluctuation >=) pxGaps


pixelGap :: (Pixel p, RealFrac g) => p -> p -> g
pixelGap p1 p2  = pxGap
    where
        (red1, green1, blue1) = rgb p1
        (red2, green2, blue2) = rgb p2
        r1 = fromIntegral red1
        g1 = fromIntegral green1
        b1 = fromIntegral  blue1
        r2 = fromIntegral red2
        g2 = fromIntegral green2
        b2 = fromIntegral  blue2
        zeroOrRatio a b = if a /= 0 && b /= 0
                            then a / b
                            else 0
        assignComps a b c = if
            | a > b && b > c -> (a + b, zeroOrRatio b a, 0, 0)
            | a > c && c > b -> (0, 0, a + c, zeroOrRatio c a)
            | otherwise -> (0, 0, 0, 0)

        (sbr1, br1, sbg1, bg1) = assignComps b1 r1 g1
        (sgb1, gb1, sgr1, gr1) = assignComps g1 b1 r1
        (srg1, rg1, srb1, rb1) = assignComps r1 g1 b1

        (sbr2, br2, sbg2, bg2) = assignComps b2 r2 g2
        (sgb2, gb2, sgr2, gr2) = assignComps g2 b2 r2
        (srg2, rg2, srb2, rb2) = assignComps r2 g2 b2

        brbg = 0.354862745
        bggb = 0.996078431
        gbgr = 0.505882353
        grrg = 0.996078431
        rgrb = 0.505882353
        pxGap = if
            | sbr1 > 0 && sbr2 > 0 && br1 > 0 && br2 > 0 -> if | br1 /= br2 -> abs(br2 - br1)
                                                               | br1 == br2 && br1 == 255 -> 1000
                                                               | otherwise -> 0
            | sbr1 > 0 && sbg2 > 0 && br1 < 0.44 && bg2 < 0.54 -> br1 - brbg + bg2 - brbg


            | sbg1 > 0 && sbg2 > 0 && bg1 > 0 && bg2 > 0 -> if | bg1 /= bg2 -> abs(bg2 - bg1)
                                                               | bg1 == bg2 && bg1 == 255 -> 1000
                                                               | otherwise -> 0
            | sbg1 > 0 && sgb2 > 0 && bg1 > 0.8 && gb2 > 0.8 -> bggb - bg1 + bggb - gb2
            | sbg1 > 0 && sbr2 > 0 && bg1 < 0.54 && br2 < 0.44 -> bg1 - brbg + br2 - brbg


            | sgb1 > 0 && sgb2 > 0 && gb1 > 0 && gb2 > 0 -> if | gb1 /= gb2 -> abs(gb2 - gb1)
                                                               | gb1 == gb2 && gb1 == 255 -> 1000
                                                               | otherwise -> 0
            | sgb1 > 0 && sbg2 > 0 && gb1 > 0.8 && bg2 > 0.8 -> bggb - gb1 + bggb - bg2
            | sgb1 > 0 && sgr2 > 0 && gb1 < 0.7 && gr2 < 0.7 -> gb1 - gbgr + gr2 - gbgr

            | sgr1 > 0 && sgr2 > 0 && gr1 > 0 && gr2 > 0 -> if | gr1 /= gr2 -> abs(gr2 - gr1)
                                                               | gr1 == gr2 && gb1 == 255 -> 1000
                                                               | otherwise -> 0
            | sgr1 > 0 && sgb2 > 0 && gr1 < 0.7 && gb2 < 0.7 -> gr1 - gbgr + gb2 - gbgr
            | sgr1 > 0 && srg2 > 0 && gr1 > 0.8 && rg2 > 0.8 -> grrg - gr1 + grrg - rg2

            | srg1 > 0 && srg2 > 0 && rg1 > 0 && rg2 > 0 -> if | rg1 /= rg2 -> abs(rg2 - rg1)
                                                               | rg1 == rg2 && rg1 == 255 -> 1000
                                                               | otherwise -> 0
            | srg1 > 0 && sgr2 > 0 && rg1 > 0.8 && gr2 > 0.8 -> grrg - gr2 + grrg - rg1
            | srg1 > 0 && srb2 > 0 && rg1 < 0.7 && rb2 < 0.7 -> rg1 - rgrb + rb2 - rgrb

            | srb1 > 0 && srb2 > 0 && rb1 > 0 && rb2 > 0 -> if | rb1 /= rb2 -> abs(rb2 - rb1)
                                                               | rb1 == rb2 && rb1 == 255 -> 1000
                                                               | otherwise -> 0
            | srb1 > 0 && srg2 > 0 && rg2 < 0.7 && rb1 < 0.7 -> rg2 - rgrb + rb1 - rgrb

            | otherwise -> 10000
