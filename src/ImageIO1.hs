{-# LANGUAGE TypeFamilies #-}

module ImageIO1 where

import qualified Codec.Picture as JP
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Word ( Word8 )

import Image1( Image(..),
               Pixel(..) )


class Pixel p => CodecPixel p where
    fromCodecPixel :: JP.PixelRGB8 -> p
    toCodecPixel :: p -> JP.PixelRGB8


newtype RGB8Pixel = RGB8Pixel Int


instance Pixel RGB8Pixel where
    type PixelComponent RGB8Pixel = Word8

    pixelComponents p =
        let (r, g, b) = toRGB p
        in [r, g, b]

    clear p = RGB8Pixel 0


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


instance CodecPixel RGB8Pixel where
    fromCodecPixel p@(JP.PixelRGB8 r g b) = fromRGB r g b

    toCodecPixel p = let (r, g, b) = toRGB p
                     in JP.PixelRGB8 r g b


-- readImage :: (Image s p, CodecPixel p) => FilePath -> IO (Either String (s p))
-- readImage fp = do
--     eimg <- JP.readImage fp
--     case eimg of
--         Left err -> return (Left $ "Could not read image: " ++ err)
--         Right dimg ->
--             return (Right (makeImage w h pf))
--             where img = JP.convertRGB8 dimg
--                   (w, h) = (JP.imageWidth img, JP.imageHeight img)
--                   pf = \x y -> fromCodecPixel (JP.pixelAt img x y)


-- writeImageAsPng :: (Image s p, CodecPixel p) => FilePath -> s p -> IO ()
-- writeImageAsPng filePath img = JP.writePng filePath $
--     JP.generateImage pf (width img) (height img)
--     where
--         pf = \x y -> toCodecPixel (pixelAt img x y)
