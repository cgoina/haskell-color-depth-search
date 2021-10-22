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
{-# LANGUAGE TypeOperators #-}


{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}


module Image ( Image
             , width, height
             , imap, imapROI
             , makeImage
             , ShiftOptions(..)
             , unsafeGetAt, unsafePixelAt
             ) where

import Control.Applicative ( Applicative(liftA2) )
import Data.Kind ( Type )
import Data.Proxy ( Proxy(..) )
import Data.Reflection
import qualified Data.Vector as V
import qualified GHC.TypeLits as TL
import qualified GHC.TypeNats as TN
import GHC.Natural
import GHC.Num (integerToNatural)

import Internal

data ShiftOptions = None | One | Two
                    deriving Show


getXyShift :: ShiftOptions -> Int
getXyShift None = 0
getXyShift One = 1
getXyShift Two = 2

-- data Image sh (n::TL.Nat) p = Image (Dim (n::TL.Nat)) (V.Vector p)

data Image :: TL.Nat -> TL.Nat -> Type -> Type where
    UnsafeImage :: Natural -> Natural -> V.Vector p -> Image w h p


width :: Image w h p -> Int
width (UnsafeImage w _ _) = fromIntegral $ naturalToInteger w
{-# INLINE width #-}


height :: Image w h p -> Int
height (UnsafeImage _ h _) = fromIntegral $ naturalToInteger h
{-# INLINE height #-}

pixels :: Image w h p -> V.Vector p
pixels (UnsafeImage _ _ ps) = ps
{-# INLINE pixels #-}


instance Functor (Image w h) where
    fmap f img@(UnsafeImage w h ps) = UnsafeImage w h $ fmap f ps


instance (TL.KnownNat w, TL.KnownNat h) => Applicative (Image w h) where
    pure = replicatePixel
    fs <*> xs = (\(f, x) -> f x) <$> zipImage fs xs


instance Foldable (Image w h) where
    -- foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f acc img@(UnsafeImage _ _ ps) = foldr f acc ps


instance Traversable (Image w h) where
    -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f img@(UnsafeImage w h ps) = UnsafeImage w h <$> traverse f ps


instance (Applicative (Image w h), Num p) => Num (Image w h p) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger i = pure $ fromInteger i


replicatePixel :: forall w h p. (TL.KnownNat w, TL.KnownNat h) =>  p -> Image w h p
replicatePixel p = UnsafeImage w h $ V.replicate sz p
    where
        w = integerToNatural $ TL.natVal (Proxy @w)
        h = integerToNatural $ TL.natVal (Proxy @h)
        sz = fromIntegral $ naturalToInteger (w * h)


zipImage :: Image w h a -> Image w h b -> Image w h (a, b)
zipImage img1@(UnsafeImage w1' h1' _) img2 =
    let (w1,h1) = (width img1, height img1)
        (w2,h2) = (width img2, height img2)
        xs = pixels img1
        ys = pixels img2

    in  if (w1,h1) == (w2,h2) then UnsafeImage w1' h1' $ V.zip xs ys
        else error "The two images must have the same shape"


imap :: (Int -> Int -> a -> b) -> Image w h a -> Image w h b
imap f img@(UnsafeImage w h ps) = UnsafeImage w h $ V.imap f' ps
    where f' i = let (y,x) = i `divMod` width img
                 in f x y


imapROI :: (Int -> Int -> a -> b)
        -> b -- value outside the ROI
        -> Image w h a
        -> (Int, Int) -- (startX, startY)
        -> (Int, Int) -- (endX, endY)
        -> Image w h b
imapROI f b img@(UnsafeImage w h ps) (startX, startY) (endX, endY) = UnsafeImage w h $ V.imap f' ps
    where f' i = if x >= startX && x < endX && y >= startY && y < endY then
                    -- if it's inside the window apply the function
                    f (x-startX) (y-startY)
                 else
                    const b
                 where (y,x) = i `divMod` width img


unsafePixelAt :: Image w h p
              -> Int -- x
              -> Int -- y
              -> p
unsafePixelAt img x y =
    unsafeGetAt img (width img * y + x)
{-# INLINE unsafePixelAt #-}


unsafeGetAt :: Image w h p
            -> Int -- | pixel index
            -> p
unsafeGetAt img@(UnsafeImage _ _ ps) i = ps V.! i
{-# INLINE unsafeGetAt #-}


makeImage :: Int -- width
          -> Int -- height
          -> (Int -> Int -> p) -- p(x,y)
          -> Image w h p
makeImage w h pf =
    let pxs = [pf x y | y <- [0..h-1], x <- [0..w-1]]
    in UnsafeImage (fromIntegral w) (fromIntegral h) (V.fromList pxs)


createUnsafeWindow :: Image w h e
                   -> Coord -- start
                   -> Dims -- size
                   -> ImageWindow e
createUnsafeWindow img start sz = ImageWindow start sz wi
    where
        wi coord =  uncurry (unsafePixelAt img) (liftCoord2 (+) start coord)
