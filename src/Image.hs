{-# LANGUAGE MultiParamTypeClasses #-}

module Image where

import Data.Word ( Word8 )


type RedC = Word8
type GreenC = Word8
type BlueC = Word8


class Pixel p where
    rgb :: p -> (RedC, GreenC, BlueC)

    makePixel :: RedC -> GreenC -> BlueC -> p

    clear :: p -> p
    clear p = makePixel 0 0 0


aboveThreshold :: (Ord a, Num a, Pixel p) => p -> a -> Bool
aboveThreshold p th = let (r, g, b) = rgb p
                      in fromIntegral r > th && fromIntegral g > th && fromIntegral b > th


class (Functor s, Pixel p) => Image s p where
    width :: s p -> Int

    height :: s p -> Int

    pixelAt :: s p -> Int -> Int -> p
    pixelAt img x y = getAt img (fromXyToIndex img (x,y))

    getAt :: s p -> Int -> p
    getAt img i = uncurry (pixelAt img) (fromIndexToXY img i)

    size :: s p -> Int
    size img = width img * height img

    fromXyToIndex :: s p -> (Int,Int) -> Int
    fromXyToIndex img (x, y) = y * width img + x

    fromIndexToXY :: s p -> Int -> (Int,Int)
    fromIndexToXY img i = let (y,x) = i `divMod` width img
                          in (x,y)

    makeImage :: Int -> Int -> (Int -> Int -> p) -> s p


-- return pixel coordinates (indexes) and values of the pixels that satisfy the predicate
regionPixelsAndCoords :: Image s p => s p -> ((Int,Int) -> (Int,Int)) -> (p -> Bool) -> [(Int, p)]
regionPixelsAndCoords img coordTransform cond =
    let w = width img
        h = height img
        outOfBounds :: (Int,Int) -> Bool -- check if coordinates are out of bounds
        outOfBounds (x, y) = x < 0 || y < 0 || x >= w || y >= h
        pis = map (fromXyToIndex img) $ 
              filter 
                (not . outOfBounds)  
                [coordTransform (x, y) | y <- [0 .. h - 1], x <- [0 .. w - 1]]
        pcs = map (getAt img) pis
    in
        filter (cond . snd) (zip pis pcs)

