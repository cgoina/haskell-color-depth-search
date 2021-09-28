{-# LANGUAGE RankNTypes #-}

module ImageProcessing1 ( clearRegion, horizontalMirror )
where

import Image1 ( Image, height, imap
              , unsafePixelAt, width )
import Pixel ( Pixel(clear) )


clearRegion :: forall w h p. Pixel p => Image w h p -> (Int -> Int -> Bool) -> Image w h p
clearRegion img regionCond = imap f img
    where f x y p = if regionCond x y then
                        clear p
                    else
                        unsafePixelAt img x y


horizontalMirror :: Image w h p -> Image w h p
horizontalMirror img = imap f img
    where
        w = width img
        f x y p = unsafePixelAt img (w - x - 1) y


shift :: forall w h p. Pixel p => Image w h p
                               -> Int -- delta x
                               -> Int -- delta y
                               -> Image w h p
shift img dx dy = imap f img
    where
        w = width img
        h = height img
        f x y p =
            let shiftedX = x + dx
                shiftedY = y + dy
            in
                if shiftedX >= 0 && shiftedX < w && shiftedY >= 0 && shiftedY < h then
                    unsafePixelAt img shiftedX shiftedY
                else
                    clear p
