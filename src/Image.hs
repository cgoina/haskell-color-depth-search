{-# LANGUAGE MultiParamTypeClasses #-}

module Image where

import Data.Word ( Word8 )
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)


type RedC = Word8
type GreenC = Word8
type BlueC = Word8


class Pixel p where
    rgb :: p -> (RedC, GreenC, BlueC)

    makePixel :: RedC -> GreenC -> BlueC -> p

    clear :: p -> p
    clear p = makePixel 0 0 0


toNum :: (Pixel p) => p -> Int
toNum p = 
    let (r,g,b) = rgb p
    in (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b



aboveThreshold :: (Pixel p, Integral a) => p -> a -> Bool
aboveThreshold p th = let (r, g, b) = rgb p
                      in r > fromIntegral th && g > fromIntegral th && b > fromIntegral th


class (Functor s, Pixel p) => Image s p where
    width :: s p -> Int

    height :: s p -> Int

    pixelAt :: s p -> Int -> Int -> p
    pixelAt img x y = getAt img (fromXyToIndex (x,y) (width img))

    getAt :: s p -> Int -> p
    getAt img i = uncurry (pixelAt img) (fromIndexToXY i (width img))

    size :: s p -> Int
    size img = width img * height img

    makeImage :: Int -> Int -> (Int -> Int -> p) -> s p


fromXyToIndex :: (Int,Int) -> Int -> Int
fromXyToIndex (x, y) w = y * w + x


fromIndexToXY :: Int -> Int -> (Int,Int)
fromIndexToXY i w = let (y,x) = i `divMod` w in (x,y)


-- return pixel coordinates (indexes) and values of the pixels that satisfy the predicate
regionPixelsAndCoords :: Image s p => s p -> ((Int,Int) -> (Int,Int)) -> (p -> Bool) -> [(Int, p)]
regionPixelsAndCoords img coordTransform cond =
    let w = width img
        h = height img
        outOfBounds :: (Int,Int) -> Bool -- check if coordinates are out of bounds
        outOfBounds (x, y) = x < 0 || y < 0 || x >= w || y >= h
        pis = map (`fromXyToIndex` w) $ 
              filter 
                (not . outOfBounds)  
                [coordTransform (x, y) | y <- [0 .. h - 1], x <- [0 .. w - 1]]
        pcs = map (getAt img) pis
    in
        filter (cond . snd) (zip pis pcs)


imagePixels :: Image s p => s p -> [p]
imagePixels img = map (getAt img) [0..(size img - 1)]
