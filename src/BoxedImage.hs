{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BoxedImage (
    BoxedImage
) where


import Control.Applicative ( Applicative(liftA2) )
import qualified Data.Vector as V

import Image ( Image(width, getAt, makeImage, height), Pixel )


data BoxedImage p = BoxedImage {
    -- | image dimensions
    dims :: !(Int, Int)
    -- | pixels vector
  , pixels :: !(V.Vector p)
}


instance Pixel p => Image BoxedImage p where
    width = fst . dims

    height = snd . dims

    getAt img@(BoxedImage _ ps) i = ps V.! i

    makeImage w h pf = BoxedImage (w, h) (V.fromList pxs) where
        pxs = [pf x y | y <- [0..h-1], x <- [0..w-1]]


instance Functor BoxedImage where
    fmap f img@(BoxedImage sz ps) = BoxedImage sz $ fmap f ps


instance Applicative BoxedImage where
    pure p = BoxedImage (1, 1) $ V.singleton p
    ffimg@(BoxedImage szff pff) <*> faimg@(BoxedImage szfa pfa)
        | szff /= szfa = error "Cannot apply images of unequal dimensions."
        | otherwise = BoxedImage szfa (V.imap func pfa)
            where
                func i = pff V.! i


instance Num a => Num (BoxedImage a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger i = pure $ fromInteger i
