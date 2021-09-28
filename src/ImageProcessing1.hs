{-# LANGUAGE RankNTypes #-}

module ImageProcessing1 ( horizontalMirror )
where

import Image1 ( Image, imap
              , width, unsafePixelAt )
import Pixel ( Pixel(clear) )


-- clearRegion :: forall w h p. Pixel p => Image w h p -> (Int -> Int -> Bool) -> Image w h p
-- clearRegion img regionCond = fmap clear img
    
    -- makeImage (width img) (height img) (\x y -> if regionCond x y then clear (pixelAt img x y) else pixelAt img x y)



horizontalMirror :: Image w h p -> Image w h p
horizontalMirror img = imap f img
    where
        w = width img
        f x y p = unsafePixelAt img (w - x - 1) y
