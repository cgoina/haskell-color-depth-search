{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module ImageIO1 where

import qualified Codec.Picture as JP
import GHC.TypeNats ( KnownNat )

import Image1( Image, makeUnsafeBoxedImage, fromUnsafeImage )
import Pixel( Pixel, RGB8Pixel, fromRGB, toRGB )


class Pixel p => CodecPixel p where
    fromCodecPixel :: JP.PixelRGB8 -> p
    toCodecPixel :: p -> JP.PixelRGB8


instance CodecPixel RGB8Pixel where
    fromCodecPixel p@(JP.PixelRGB8 r g b) = fromRGB r g b

    toCodecPixel p = let (r, g, b) = toRGB p
                     in JP.PixelRGB8 r g b


readImage :: forall w h p. (KnownNat w, KnownNat h, CodecPixel p) => FilePath
                                    -> IO (Either String (Image w h p))
readImage fp = do
    eimg <- JP.readImage fp
    case eimg of
        Left err -> return (Left $ "Could not read image: " ++ err)
        Right dimg ->
            return (Right (fromUnsafeImage unsafeImg))
            where img = JP.convertRGB8 dimg
                  (w, h) = (JP.imageWidth img, JP.imageHeight img)
                  pf x y = fromCodecPixel (JP.pixelAt img x y)
                  unsafeImg = makeUnsafeBoxedImage w h pf


-- writeImageAsPng :: (Image s p, CodecPixel p) => FilePath -> s p -> IO ()
-- writeImageAsPng filePath img = JP.writePng filePath $
--     JP.generateImage pf (width img) (height img)
--     where
--         pf = \x y -> toCodecPixel (pixelAt img x y)
