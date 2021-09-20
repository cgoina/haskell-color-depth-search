{-# LANGUAGE MultiParamTypeClasses #-}

module ColorDepthSearch 
    ( ShiftOptions(..)
    , calculateBestScore
    , createQueryMasks )
    where

import Image ( Image )

import ColorDepthSearch.Internal
    ( CDSMask( applyMask )
    , ShiftOptions(..) )

import qualified ColorDepthSearch.Naive as N ( MaskPixels,
                                               createAllMaskPixels)
import qualified ColorDepthSearch.Accelerate as A ( ImageMask,
                                                    createAllMaskPixels)

createQueryMasks :: (Image s p, Integral t) => s p -- image
                                             -> t -- threshold
                                             -> Bool -- mirror
                                             -> ShiftOptions
                                             -> [N.MaskPixels p] -- color depth masks
createQueryMasks = N.createAllMaskPixels


createQueryMasks' :: (Image s p, Integral t) => s p -- image
                                            -> t -- threshold
                                            -> Bool -- mirror
                                            -> ShiftOptions
                                            -> [A.ImageMask p] -- color depth masks
createQueryMasks' = A.createAllMaskPixels


calculateBestScore :: (CDSMask m p, Image s p, Integral t, RealFrac z) => [m p] -> s p -> t -> z -> Int
calculateBestScore queries target targetThreshold pixColorFluctuation = 
    let calcMaskScore m = applyMask m target targetThreshold pixColorFluctuation
    in foldr (max . calcMaskScore) 0 queries
