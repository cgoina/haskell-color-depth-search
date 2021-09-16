{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts  #-}

module ColorDepthSearch.Accelerate where

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native  as CPU

import Data.Array.Accelerate.Sugar.Elt

import Data.Word ( Word8 )

import Image( Image( getAt )
            , Pixel(rgb)
            , aboveThreshold )

import qualified Prelude as P

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