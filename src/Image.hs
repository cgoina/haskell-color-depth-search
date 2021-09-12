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


class (Pixel p) => Image s p where
    width :: s p -> Int

    height :: s p -> Int

    pixelAt :: s p -> Int -> Int -> p
    pixelAt img x y = let i = y * width img + x
                      in getAt img i

    getAt :: s p -> Int -> p
    getAt img i = let (y, x) = i `divMod` width img
                  in pixelAt img x y

    makeImage :: Int -> Int -> (Int -> Int -> p) -> s p





