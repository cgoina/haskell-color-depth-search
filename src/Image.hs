{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}


{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

module Image ( Image
             , dims
             , Image.width, Image.height
             , imap, imapROI
             , makeImage
             , ShiftOptions(..)
             , unsafeGetAt, unsafePixelAt
             ) where

import Control.Applicative ( Applicative(liftA2) )
import Data.Kind ( Type )
import Data.Proxy ( Proxy(..) )
import qualified Data.Vector as V

import qualified GHC.TypeLits as TL ( Nat
                                    , KnownNat
                                    , natVal )

import Dims ( Dims
            , makeDims
            , width
            , height )


data ShiftOptions = None | One | Two
                    deriving Show


getXyShift :: ShiftOptions -> Int
getXyShift None = 0
getXyShift One = 1
getXyShift Two = 2


data Image :: TL.Nat -> TL.Nat -> Type -> Type where
    Image :: Dims w h -> V.Vector p -> Image w h p


dims :: Image w h p -> Dims w h
dims (Image sz _) = sz


width :: Image w h p -> Int
width (Image sz _) = Dims.width sz


height :: Image w h p -> Int
height (Image sz _) = Dims.height sz


pixels :: Image w h p -> V.Vector p
pixels (Image _ ps) = ps



instance Functor (Image w h) where
    fmap f img@(Image sz ps) = Image sz $ fmap f ps


instance (TL.KnownNat w, TL.KnownNat h) => Applicative (Image w h) where
    pure = replicatePixel
    fs <*> xs = (\(f, x) -> f x) <$> zipImage fs xs


instance Foldable (Image w h) where
    -- foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f acc img@(Image _ ps) = foldr f acc ps


instance Traversable (Image w h) where
    -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f img@(Image sz ps) = Image sz <$> traverse f ps


instance (TL.KnownNat w, TL.KnownNat h, Num p) => Num (Image w h p) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger i = pure $ fromInteger i


replicatePixel :: forall w h p. (TL.KnownNat w, TL.KnownNat h) => p -> Image w h p
replicatePixel p = Image sz $ V.replicate (Dims.width sz * Dims.height sz) p
  where
    w1 = fromIntegral (TL.natVal (Proxy @w))
    h1 = fromIntegral (TL.natVal (Proxy @h))
    sz = makeDims @w @h w1 h1


imap :: (Int -> Int -> a -> b) -> Image w h a -> Image w h b
imap f img@(Image sz ps) = Image sz $ V.imap f' ps
    where f' i = let (y,x) = (i `divMod` Dims.width sz)
                 in f x y


imapROI :: (Int -> Int -> a -> b)
        -> b -- value outside the ROI
        -> Image w h a
        -> (Int, Int) -- (startX, startY)
        -> (Int, Int) -- (endX, endY)
        -> Image w h b
imapROI f b img@(Image sz ps) (startX, startY) (endX, endY) = Image sz $ V.imap f' ps
    where f' i = if x >= startX && x < endX && y >= startY && y < endY then
                    -- if it's inside the window apply the function
                    f (x-startX) (y-startY)
                 else
                    const b
                 where (y,x) = i `divMod` Dims.width sz


zipImage :: Image w h a -> Image w h b -> Image w h (a, b)
zipImage img1@(Image sza xs) img2@(Image szb ys) = Image sza (V.zip xs ys)


{-# INLINE unsafePixelAt #-}
unsafePixelAt :: Image w h p
              -> Int -- x
              -> Int -- y
              -> p
unsafePixelAt img@(Image sz _) x y = unsafeGetAt img (Dims.width sz * Dims.height sz + x)


{-# INLINE unsafeGetAt #-}
unsafeGetAt :: Image w h p
            -> Int -- | pixel index
            -> p
unsafeGetAt img@(Image _ ps) i = ps V.! i


makeImage :: Int
          -> Int
          -> (Int -> Int -> p)
          -> Image w h p
makeImage w h pf =
    let pxs = [pf x y | y <- [0..h-1], x <- [0..w-1]]
    in Image (makeDims w h) (V.fromList pxs)
