{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

module Image1 where

import Control.Applicative ( Applicative(liftA2) )
import Data.Finite
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Singletons (SomeSing(..), Sing, SingI, toSing, withSingI)
import Data.Singletons.TH ( genSingletons, singletons, withSing )
import qualified Data.Vector as V
import Data.Coerce (Coercible, coerce)
import GHC.TypeNats (Nat, KnownNat, natVal, someNatVal)

$(genSingletons [''Nat])

class Pixel p where
    type PixelComponent p :: *

    pixelComponents :: p -> [PixelComponent p]

    clear :: p -> p


data Image (w::Nat) (h::Nat) p where
    UnsafeImage :: { pixels :: !(V.Vector p) } -> Image w h p


instance Functor (Image w h) where
    fmap f img@(UnsafeImage ps) = UnsafeImage $ fmap f ps


instance (KnownNat w, KnownNat h) => Applicative (Image w h) where
    pure = replicatePixel
    fs <*> xs = (\(f, x) -> f x) <$> zipImage fs xs


instance (KnownNat w, KnownNat h, Num p) => Num (Image w h p) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger i = pure $ fromInteger i


replicatePixel :: forall w h p. (KnownNat w, KnownNat h) => p -> Image w h p
replicatePixel p = UnsafeImage $ V.replicate (w1*h1) p
  where
    w1 = fromIntegral (natVal (Proxy @w))
    h1 = fromIntegral (natVal (Proxy @h))


zipImage :: Image w h a -> Image w h b -> Image w h (a, b)
zipImage (UnsafeImage xs) (UnsafeImage ys) = UnsafeImage (V.zip xs ys)


dims :: forall w h p. (KnownNat w, KnownNat h) => Image w h p -> (Finite w, Finite h)
dims img = (w1, h1)
  where
    w1 = finite $ fromIntegral (natVal (Proxy @w))
    h1 = finite $ fromIntegral (natVal (Proxy @h))


width :: forall w h p. (KnownNat w, KnownNat h) => Image w h p -> Finite w
width = fst . dims


height :: forall w h p. (KnownNat w, KnownNat h) => Image w h p -> Finite h
height = snd . dims


pixelAt :: forall w h p. (KnownNat w, KnownNat h) => Image w h p
                                                  -> Finite w
                                                  -> Finite h
                                                  -> p
pixelAt img x y =
    let w = fromIntegral (getFinite (width img))
        x' = fromIntegral (getFinite x)
        y' = fromIntegral (getFinite y)
        i = (w * y' + x')
    in unsafeIndex img i

{-# INLINE unsafeIndex #-}
unsafeIndex :: Image w h p -> Int -> p
unsafeIndex img@(UnsafeImage ps) i = ps V.! i

makeImage_ :: Sing w -> Sing h -> V.Vector p -> Image w h p
makeImage_ _ _ = UnsafeImage


withVect
  :: Nat
  -> Nat
  -> V.Vector p
  -> (forall (x :: Nat) (y :: Nat). (SingI x, SingI y) => Image x y p -> k)
  -> k
withVect x y ps f =
    case toSing y of
        SomeSing (sy :: Sing m) -> withSingI sy $
            case toSing x of
                SomeSing (sx :: Sing n) -> withSingI sx $ f (makeImage_ sx sy ps)


makeImage :: Finite w -> Finite h -> V.Vector p -> Image w h p
makeImage x y = UnsafeImage

makeImage1 :: forall w h p. (KnownNat w, KnownNat h) => Int -> Int -> V.Vector p -> Image w h p
makeImage1 x y = makeImage (finite (fromIntegral x)) (finite (fromIntegral y))