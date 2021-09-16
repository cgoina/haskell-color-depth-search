{-# LANGUAGE MultiWayIf #-}

module ColorDepthSearch where

import Image( Image )

import ColorDepthSearch.Accelerate ( calculateScore )


calculateBestScore :: (Num t, Ord t, RealFrac z, Image s p) => [[(Int,p)]] -> s p -> t -> z -> Int
calculateBestScore queries target targetThreshold pixColorFluctuation = 
    let calcMaskScore = \m -> calculateScore m target targetThreshold pixColorFluctuation
    in maximum $ map calcMaskScore queries

