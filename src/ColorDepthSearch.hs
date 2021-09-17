{-# LANGUAGE MultiParamTypeClasses #-}

module ColorDepthSearch 
    ( ShiftOptions(..)
    , calculateBestScore
    , createQueryMasks )
    where

import Image ( Image )

import ColorDepthSearch.Internal
    ( CDSMask(createAllMasks)
    , ShiftOptions(..)
    , calculateBestScore )

import ColorDepthSearch.Naive ( createAllMaskPixels, MaskPixels ) 

createQueryMasks :: (Image s p, Ord t, Num t) => s p -- image
                                                           -> t -- threshold
                                                           -> Bool -- mirror
                                                           -> ShiftOptions
                                                           -> [MaskPixels p] -- color depth masks
createQueryMasks = createAllMaskPixels