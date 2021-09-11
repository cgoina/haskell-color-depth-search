{-# LANGUAGE MultiParamTypeClasses #-}

module Image where

import Data.Word ( Word8 )
import Data.Bits ((.&.), (.|.), shiftL, shiftR)

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

    makeImage :: Int -> Int -> (Int -> Int -> p) -> s p

    pixelAt :: s p -> Int -> Int -> p

    pixelAt img x y = let i = y * width img + x
                      in getAt img i

    getAt :: s p -> Int -> p
    getAt img i = let (y, x) = i `divMod` width img
                  in pixelAt img x y

    clearRegion :: s p -> (Int -> Int -> Bool) -> s p
    clearRegion img region = makeImage (width img) (height img) (\x y -> if region x y then clear (pixelAt img x y) else pixelAt img x y)


instance Pixel Int where
    rgb p = let r = fromIntegral $ (p `shiftR` 16) .&. 0xFF
                g = fromIntegral $ (p `shiftR` 8) .&. 0xFF
                b = fromIntegral $ p .&. 0xFF
            in (r, g, b)

    makePixel r g b = (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

    clear p = 0
