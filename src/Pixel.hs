{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fprint-potential-instances #-}

module Pixel ( Pixel(..), RGB8Pixel, fromRGB, toRGB ) where

import Data.Word ( Word8 )
import Data.Bits ((.&.), (.|.), shiftL, shiftR)

class (Ord p, Eq p) => Pixel p where
    type PixelComponent p :: *

    emptyPixel :: p

    pixelComponents :: p -> [PixelComponent p]

    clear :: p -> p
    clear p = emptyPixel


newtype RGB8Pixel = RGB8Pixel Int deriving (Ord, Eq)


instance Pixel RGB8Pixel where
    type PixelComponent RGB8Pixel = Word8

    pixelComponents p =
        let (r, g, b) = toRGB p
        in [r, g, b]

    emptyPixel = RGB8Pixel 0


instance Num RGB8Pixel where
  (+) p1 p2 = let (r1, g1, b1) = toRGB p1
                  (r2, g2, b2) = toRGB p2
              in fromRGB (r1+r2) (g1+g2) (b1+b2)
  (-) p1 p2 = let (r1, g1, b1) = toRGB p1
                  (r2, g2, b2) = toRGB p2
              in fromRGB (r1-r2) (g1-g2) (b1-b2)
  (*) p1 p2 = let (r1, g1, b1) = toRGB p1
                  (r2, g2, b2) = toRGB p2
              in fromRGB (r1*r2) (g1*g2) (b1*b2)
  abs p = let (r, g, b) = toRGB p
          in fromRGB (abs r) (abs g) (abs b)
  signum p = let (r, g, b) = toRGB p
             in fromRGB (signum r) (signum g) (signum b)
  fromInteger = RGB8Pixel . fromIntegral


fromRGB :: Word8 -> Word8 -> Word8 -> RGB8Pixel
fromRGB r g b =
    let p = (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b
    in RGB8Pixel p


toRGB :: RGB8Pixel -> (Word8, Word8, Word8)
toRGB (RGB8Pixel p) =
        let r = fromIntegral $ (p `shiftR` 16) .&. 0xFF
            g = fromIntegral $ (p `shiftR` 8) .&. 0xFF
            b = fromIntegral $ p .&. 0xFF
        in (r, g, b)

