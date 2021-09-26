{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fprint-potential-instances #-}

module Image1 where

import Data.Kind (Type)
import Data.Proxy
import GHC.TypeNats
import qualified Data.Vector as V

class Pixel p where
    type PixelComponent p :: *

    pixelComponents :: p -> [PixelComponent p]

    clear :: p -> p


data Image (w::Nat) (h::Nat) p where
    UnsafeImage :: { pixels :: !(V.Vector p) } -> Image w h p


instance Functor (Image w h) where
    fmap f img@(UnsafeImage ps) = UnsafeImage $ fmap f ps


replicate :: forall w h p. (KnownNat w, KnownNat h) => p -> Image w h p
replicate p = UnsafeImage $ V.replicate (w1*h1) p
  where
    w1 = fromIntegral (natVal (Proxy @w))
    h1 = fromIntegral (natVal (Proxy @h))


dims :: forall w h p. (KnownNat w, KnownNat h) => Image w h p -> (Int,Int)
dims img = (w1, h1)
  where
    w1 = fromIntegral (natVal (Proxy @w))
    h1 = fromIntegral (natVal (Proxy @h))
