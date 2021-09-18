{-# LANGUAGE MultiParamTypeClasses #-}

module ColorDepthSearch.Internal 
    ( ShiftOptions(..)
    , getXyShift
    , CDSMask( createAllMasks, applyMask )
    , calculateBestScore )
    where

import Image ( Image
             , Pixel )

data ShiftOptions = None | One | Two
                    deriving Show


getXyShift :: ShiftOptions -> Int
getXyShift None = 0
getXyShift One = 1
getXyShift Two = 2


class Pixel p => CDSMask m p where
    createAllMasks :: (Image s p, Integral t) => s p -- image
                                                -> t -- threshold
                                                -> Bool -- mirror
                                                -> ShiftOptions
                                                -> [m p] -- color depth masks
    applyMask :: (Image s p, Integral t, RealFrac z) => m p -- mask
                                                     -> s p -- image
                                                     -> t -- target threshold
                                                     -> z -- pixel color fluctuation
                                                     -> Int -- cds score


calculateBestScore :: (CDSMask m p, Image s p, Integral t, RealFrac z) => [m p] -> s p -> t -> z -> Int
calculateBestScore queries target targetThreshold pixColorFluctuation = 
    let calcMaskScore m = applyMask m target targetThreshold pixColorFluctuation
    in maximum $ map calcMaskScore queries
