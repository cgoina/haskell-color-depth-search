module ImageIO where

import qualified Codec.Picture as JP
import Data.Bits ((.&.), (.|.), shiftL, shiftR)

import Image( Image(..),
              Pixel(..) )


class (Pixel p) => CodecPixel p where
    fromCodecPixel :: JP.PixelRGB8 -> p
    toCodecPixel :: p -> JP.PixelRGB8


instance Pixel JP.PixelRGB8 where
    rgb p@(JP.PixelRGB8 r g b) = (r, g, b)

    clear = JP.colorMap (const 0)

    makePixel = JP.PixelRGB8


instance CodecPixel JP.PixelRGB8 where
    fromCodecPixel = id

    toCodecPixel = id


instance Pixel Int where
    rgb p = let r = fromIntegral $ (p `shiftR` 16) .&. 0xFF
                g = fromIntegral $ (p `shiftR` 8) .&. 0xFF
                b = fromIntegral $ p .&. 0xFF
            in (r, g, b)

    makePixel r g b = (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

    clear p = 0


instance CodecPixel Int where
    fromCodecPixel p@(JP.PixelRGB8 r g b) = makePixel r g b

    toCodecPixel p = let (r, g, b) = rgb p
                     in JP.PixelRGB8 r g b


readImage :: (Image s p, CodecPixel p) => FilePath -> IO (Either String (s p))
readImage fp = do
    eimg <- JP.readImage fp
    case eimg of
        Left err -> return (Left $ "Could not read image: " ++ err)
        Right dimg ->
            return (Right (makeImage w h pf))
            where img = JP.convertRGB8 dimg
                  (w, h) = (JP.imageWidth img, JP.imageHeight img)
                  pf = \x y -> fromCodecPixel (JP.pixelAt img x y)


writeImageAsPng :: (Image s p, CodecPixel p) => FilePath -> s p -> IO ()
writeImageAsPng filePath img = JP.writePng filePath $
    JP.generateImage pf (width img) (height img)
    where
        pf = \x y -> toCodecPixel (pixelAt img x y)
