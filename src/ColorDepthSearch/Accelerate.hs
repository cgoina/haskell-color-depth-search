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
import Data.Coerce ( coerce )

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
        score = A.toList
                P.$ CPU.run
                P.$ calculatePixelsScore' mask' mTh' target' tTh'
    in case score of
      [] -> 0
      n : ns -> n


calculatePixelsScore' :: A.Acc (A.Vector A.Int)
                      -> A.Exp A.Int
                      -> A.Acc (A.Vector A.Int)
                      -> A.Exp A.Int
                      -> A.Acc (A.Scalar A.Int)
calculatePixelsScore' mask mTh target tTh =
    A.fold (A.+) 0 (A.zipWith (cdMatch mTh tTh) mask target)


cdMatch :: A.Exp A.Int
        -> A.Exp A.Int
        -> A.Exp A.Int
        -> A.Exp A.Int
        -> A.Exp A.Int
cdMatch t1 t2 p1 p2 =
    let c1 = p1 A.>= t1  A.|| p2 A.>= t2

    in A.cond c1 0 1
