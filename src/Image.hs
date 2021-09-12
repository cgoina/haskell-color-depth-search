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
    pixelAt img x y = let i = y * width img + x
                      in getAt img i

    getAt :: s p -> Int -> p
    getAt img i = let (y, x) = i `divMod` width img
                  in pixelAt img x y

    makeImage :: Int -> Int -> (Int -> Int -> p) -> s p


size :: Image s p => s p -> Int
size img = width img * height img


regionPixelCoord :: Image s b => s b -> (b -> Bool) -> [Int]
regionPixelCoord img cond = map fst (filter (cond . snd) [(i, getAt img i) | i <- [0..size img - 1]])

