{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module ColorDepthSearch.Internal 
    ( ShiftOptions(..)
    , getXyShift
    , CDSMask( createAllMasks, applyMask ) )
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
