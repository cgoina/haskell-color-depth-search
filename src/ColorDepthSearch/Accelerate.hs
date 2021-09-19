{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ColorDepthSearch.Accelerate (
    ImageMask
  , createAllMaskPixels
) where

import qualified Prelude as P

import qualified Data.Array.Accelerate as A
import qualified Data.Bits as B (Bits, (.&.), (.|.), shiftL, shiftR)

import Data.Array.Accelerate.LLVM.Native as CPU ( run )

import Data.Word ( Word8 )

import Image( Image( getAt, width, height )
            , Pixel(rgb, makePixel, clear)
            , aboveThreshold
            , imagePixels
            , toNum )
import ImageProcessing (horizontalMirror, shift)
import ColorDepthSearch.Internal ( CDSMask(..)
                                 , getXyShift
                                 , ShiftOptions )
import qualified Data.Array.Accelerate.Data.Bits as A


data ImageMask p = forall t. P.Integral t => ImageMask {
    maskPixels :: A.Vector A.Int
  , imageWidth :: P.Int
  , imageHeight :: P.Int
  , maskThreshold :: t
  , mirror :: P.Bool
  , xyShift :: (P.Int, P.Int)
}


instance Pixel p => CDSMask ImageMask p where
    createAllMasks = createAllMaskPixels
    applyMask = applyPixelsMask


getPixelsAsVector :: Image s p => s p -> A.Vector A.Int
getPixelsAsVector img =
    let w = width img
        h = height img
        pixelsFromImage = P.map toNum (imagePixels img)
    in
        A.fromList (A.Z A.:. w P.* h) pixelsFromImage


mkImageMask :: (Image s p, P.Integral t) => s p -- image
                                         -> t -- threshold
                                         -> P.Bool -- mirror
                                         -> (P.Int, P.Int) -- xyShift
                                         -> ImageMask p -- color depth masks
mkImageMask img threshold mirror xyShift =
    let maskPixels = getPixelsAsVector img
    in ImageMask maskPixels (width img) (height img) threshold mirror xyShift


createAllMaskPixels :: (Image s p, P.Integral t) => s p -- image
                                                 -> t -- threshold
                                                 -> P.Bool -- mirror
                                                 -> ShiftOptions
                                                 -> [ImageMask p] -- color depth masks
createAllMaskPixels img maskThreshold mirror pixelShift =
    let xyShift = getXyShift pixelShift
        xyShifts = [(dx,dy) | dy <- [-xyShift..xyShift], dx <- [-xyShift..xyShift]]
        masks = P.map (mkImageMask img maskThreshold P.False) xyShifts
        mirrorMasks =
            if mirror then
                P.map (mkImageMask img maskThreshold P.True) xyShifts
            else []
    in
        masks P.++ mirrorMasks


applyPixelsMask :: ( Image s p
                   , P.Integral t
                   , P.RealFrac z) => ImageMask p
                                   -> s p
                                   -> t
                                   -> z
                                   -> P.Int
applyPixelsMask mask@(ImageMask mps w h maskTh mirror dxy) targetImage targetThreshold pixColorFluctuation =
    let targetPixels = getPixelsAsVector targetImage
        (maskTh'::P.Int) = P.fromIntegral maskTh
        (targetTh'::P.Int) = P.fromIntegral targetThreshold
        (pxFluct::P.Double) = P.realToFrac pixColorFluctuation
    in
        calculatePixelsScore mps maskTh' targetPixels targetTh' pxFluct


calculatePixelsScore :: A.Vector A.Int -- mask
                     -> A.Int
                     -> A.Vector A.Int -- target
                     -> A.Int
                     -> A.Double
                     -> A.Int
calculatePixelsScore mask mTh target tTh pxColorFluctuation =
    let
        mask' = A.use mask
        target' = A.use target
        mTh' = A.constant mTh
        tTh' = A.constant tTh
        pxFluct = A.constant pxColorFluctuation
        score = A.toList
                P.$ CPU.run
                P.$ calculatePixelsScore' mask' mTh' target' tTh' pxFluct
    in case score of
        [] -> 0
        n : ns -> n


calculatePixelsScore' :: A.Acc (A.Vector A.Int)
                      -> A.Exp A.Int
                      -> A.Acc (A.Vector A.Int)
                      -> A.Exp A.Int
                      -> A.Exp A.Double
                      -> A.Acc (A.Scalar A.Int)
calculatePixelsScore' mask mTh target tTh pxFluct =
    A.fold (A.+) 0 (A.zipWith (cdMatch mTh tTh pxFluct) mask target)


cdMatch :: A.Exp A.Int
        -> A.Exp A.Int
        -> A.Exp A.Double
        -> A.Exp A.Int
        -> A.Exp A.Int
        -> A.Exp A.Int
cdMatch t1 t2 pxFluct p1 p2 =
    let (r1,g1,b1) = toRGB p1
        (r2,g2,b2) = toRGB p2

        (sbr1, br1, sbg1, bg1) = assignComps b1 r1 g1
        (sgb1, gb1, sgr1, gr1) = assignComps g1 b1 r1
        (srg1, rg1, srb1, rb1) = assignComps r1 g1 b1

        (sbr2, br2, sbg2, bg2) = assignComps b2 r2 g2
        (sgb2, gb2, sgr2, gr2) = assignComps g2 b2 r2
        (srg2, rg2, srb2, rb2) = assignComps r2 g2 b2

        brbg = A.constant (0.354862745::A.Double)
        bggb = A.constant (0.996078431::A.Double)
        gbgr = A.constant (0.505882353::A.Double)
        grrg = A.constant (0.996078431::A.Double)
        rgrb = A.constant (0.505882353::A.Double)

        pxGap = A.cond 
                (sbr1 A.> 0 A.&& sbr2 A.> 0 A.&& br1 A.> 0 A.&& br2 A.> 0 A.&& br1 A./= br2)
                (gapAB br1 br2)
                (A.cond 
                    (sbr1 A.> 0 A.&& sbg2 A.> 0 A.&& br1 A.< 0.44 A.&& bg2 A.< 0.54)
                    (br1 A.- brbg A.+ bg2 A.- brbg)
                    (A.constant 10000)
                )
                

    in A.cond (pxGap A.< pxFluct) 1 0


toRGB :: A.Exp A.Int -> (A.Exp A.Double, A.Exp A.Double, A.Exp A.Double)
toRGB c = let r = A.fromIntegral A.$ (c `A.shiftR` 16) A..&. 0xFF
              g = A.fromIntegral A.$ (c `A.shiftR` 8) A..&. 0xFF
              b = A.fromIntegral A.$ c A..&. 0xFF
        in (r, g, b)

ratio :: (A.Eq t, P.Fractional (A.Exp t)) => A.Exp t
                                          -> A.Exp t
                                          -> A.Exp t
ratio a b = A.cond (a A./= 0 A.&& b A./= 0) (a A./ b) 0

assignComps :: A.Exp A.Double
            -> A.Exp A.Double
            -> A.Exp A.Double
            -> ( A.Exp A.Double
               , A.Exp A.Double
               , A.Exp A.Double
               , A.Exp A.Double )
assignComps a b c =
    let ab = A.cond (a A.> b A.&& b A.> c)
                (A.lift (a A.+ b, ratio b a))
                (A.lift (A.constant 0, A.constant 0))
        ac = A.cond (a A.> c A.&& c A.> b)
                (A.lift (a A.+ c, ratio c a))
                (A.lift (A.constant 0, A.constant 0))
        (abSum, baRatio) = A.unlift ab
        (acSum, caRatio) = A.unlift ac

    in  (abSum, baRatio, acSum, caRatio)

gapAB :: A.Exp A.Double -> A.Exp A.Double -> A.Exp A.Double
gapAB a b = A.cond 
            (a A./= b) 
            (A.abs(a A.- b))
            (A.cond (a A.== 255) (A.constant 1000) (A.constant 0))