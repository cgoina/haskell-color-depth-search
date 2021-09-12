{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ArrayImage (
    ArrayImage
) where

import Control.Applicative ( Applicative(liftA2) )
import qualified Data.Massiv.Array as A

import Image ( Image(width, pixelAt, makeImage, height), Pixel )


newtype ArrayImage p = ArrayImage {
    pixels :: A.Array A.D A.Ix2 p
}


instance (Pixel p) => Image ArrayImage p where
    width (ArrayImage ps) = let A.Sz2 w _ = A.size ps
                                in w

    height (ArrayImage ps) = let A.Sz2 _ h = A.size ps
                                 in h

    pixelAt (ArrayImage ps) x y = A.evaluate' ps (x A.:. y)


    makeImage w h pf = ArrayImage arr where
         arr = A.makeArray A.Par (A.Sz2 w h) apf
         apf (x A.:. y) = pf x y


instance Functor ArrayImage where
    fmap f img@(ArrayImage ps) = ArrayImage $ fmap f ps


instance Applicative ArrayImage where
    pure p = ArrayImage $ A.singleton p
    ffimg@(ArrayImage pff) <*> faimg@(ArrayImage pfa) = ArrayImage $ pff <*> pfa


instance Num a => Num (ArrayImage a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger i = pure $ fromInteger i
