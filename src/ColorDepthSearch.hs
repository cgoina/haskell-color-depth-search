{-# LANGUAGE MultiParamTypeClasses #-}

module ColorDepthSearch 
    ( ShiftOptions(..)
    , calculateBestScore
    , createQueryMasks )
    where

import Image ( Image )

import ColorDepthSearch.Internal
    ( CDSMask
    , ShiftOptions(..)
    , calculateBestScore )

import qualified ColorDepthSearch.Naive as N ( MaskPixels,
                                               createAllMaskPixels)
import qualified ColorDepthSearch.Accelerate as A ( ImageMask,
                                                    createAllMaskPixels)

createQueryMasks' :: (Image s p, Ord t, Num t) => s p -- image
                                              -> t -- threshold
                                              -> Bool -- mirror
                                              -> ShiftOptions
                                              -> [N.MaskPixels p] -- color depth masks
createQueryMasks' = N.createAllMaskPixels


createQueryMasks :: (Image s p, Ord t, Num t) => s p -- image
                                              -> t -- threshold
                                              -> Bool -- mirror
                                              -> ShiftOptions
                                              -> [A.ImageMask p] -- color depth masks
createQueryMasks = A.createAllMaskPixels
