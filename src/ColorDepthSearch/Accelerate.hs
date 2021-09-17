{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module ColorDepthSearch.Accelerate where

import qualified Prelude as P

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native as CPU ( run )

import Data.Word ( Word8 )

import Image( Image( getAt, size )
            , Pixel(rgb)
            , aboveThreshold
            , imagePixels )
import ImageProcessing (horizontalMirror, shift)
import ColorDepthSearch.Internal ( CDSMask(..), getXyShift, ShiftOptions )


data ImageMask p = forall t s p. (P.Ord t, P.Num t, Image s p) => ImageMask {
    maskImage :: s p
  , maskThreshold :: t
  , mirror :: P.Bool
  , xyShift :: (P.Int, P.Int)
}


mkImageMask :: (Image s p, P.Ord t, P.Num t) => s p -- image
                                             -> t -- threshold
                                             -> P.Bool -- mirror
                                             -> (P.Int, P.Int) -- xyShift
                                             -> ImageMask p -- color depth masks
mkImageMask = ImageMask


createAllMaskPixels :: (Image s p, P.Ord t, P.Num t) => s p -- image
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


applyPixelsMask :: (Image s p, P.Ord t, P.Num t, P.RealFrac z) => ImageMask p
                                                         -> s p
                                                         -> t
                                                         -> z
                                                         -> P.Int
applyPixelsMask mask targetImage targetThreshold pixColorFluctuation = 0


calculateScore :: (P.Num t, P.Ord t, P.RealFrac z, Image s p) => [(P.Int, p)] -> s p -> t -> z -> P.Int
calculateScore mask targetImage targetThreshold pixColorFluctuation =
    let r = CPU.run P.$ calculateScore' mask targetImage targetThreshold pixColorFluctuation
    in P.head P.$ A.toList r


calculateScore' :: (P.Num t, P.Ord t, P.RealFrac z, Image s p) => [(P.Int, p)] -> s p -> t -> z -> A.Acc (A.Scalar A.Int)
calculateScore' mask targetImage targetThreshold pixColorFluctuation =
    let queryPixelsPos = P.map P.fst mask
        queryPixelsComps = P.map (rgb P.. P.snd) mask
        targetPixelsComps = P.map (rgb P.. getAt targetImage) queryPixelsPos
        accQueryPixels = A.fromList (A.Z A.:. P.length queryPixelsComps) queryPixelsComps
        accTargetPixels = A.fromList (A.Z A.:. P.length targetPixelsComps) targetPixelsComps

    in score accQueryPixels accTargetPixels


score :: A.Num a => A.Vector (a,a,a)
               -> A.Vector (a,a,a)
               -> A.Acc (A.Scalar A.Int)
score xs ys =
  let
      xs' = A.use xs
      ys' = A.use ys
  in
    A.fold (A.+) 0 ( A.zipWith cdMatch xs' ys' )


cdMatch :: A.Num a => A.Exp (a,a,a)
                   -> A.Exp (a,a,a)
                   -> A.Exp A.Int
cdMatch p1 p2 =
    1