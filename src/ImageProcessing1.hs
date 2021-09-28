{-# LANGUAGE RankNTypes #-}

module ImageProcessing1 where

import Image1 ( Image )
import Pixel ( Pixel(clear) )


-- clearRegion :: forall w h p. Pixel p => Image w h p -> (Int -> Int -> Bool) -> Image w h p
-- clearRegion img regionCond = fmap clear img
    
    -- makeImage (width img) (height img) (\x y -> if regionCond x y then clear (pixelAt img x y) else pixelAt img x y)
