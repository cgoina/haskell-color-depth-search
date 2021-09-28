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

module Image1 ( Image, UnsafeBoxedImage(UnsafeBoxedImage)
              , makeUnsafeBoxedImage
              , fromUnsafeImage
              , width, height, dims
              ) where

import Control.Applicative ( Applicative(liftA2) )
import Data.Finite
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Singletons ( SomeSing(..), Sing, SingI
                       , fromSing, toSing, withSingI, withSing, SingKind (fromSing))

import qualified Data.Vector as V
import qualified GHC.TypeNats as TN ( Nat, KnownNat
                                     , natVal )
import qualified GHC.TypeLits.Singletons as TL ( SNat )


data Image (w::TN.Nat) (h::TN.Nat) p where
    UnsafeImage :: {
        width :: Int
      , height :: Int
      , pixels :: !(V.Vector p) } -> Image w h p


instance Functor (Image w h) where
    fmap f img@(UnsafeImage cols rows ps) = UnsafeImage cols rows $ fmap f ps


instance (TN.KnownNat w, TN.KnownNat h) => Applicative (Image w h) where
    pure = replicatePixel
    fs <*> xs = (\(f, x) -> f x) <$> zipImage fs xs


instance (TN.KnownNat w, TN.KnownNat h, Num p) => Num (Image w h p) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger i = pure $ fromInteger i


replicatePixel :: forall w h p. (TN.KnownNat w, TN.KnownNat h) => p -> Image w h p
replicatePixel p = UnsafeImage w1 h1 $ V.replicate (w1*h1) p
  where
    w1 = fromIntegral (TN.natVal (Proxy @w))
    h1 = fromIntegral (TN.natVal (Proxy @h))


zipImage :: Image w h a -> Image w h b -> Image w h (a, b)
zipImage img1@(UnsafeImage wa ha xs) img2@(UnsafeImage wb hb ys) = UnsafeImage wa ha (V.zip xs ys)


{-# INLINE dims #-}
dims :: Image w h p -> (Int, Int)
dims img@(UnsafeImage w h _) = (w, h)


{-# INLINE unsafePixelAt #-}
unsafePixelAt :: Image w h p -> Int -> Int -> p
unsafePixelAt img@(UnsafeImage w _ _) x y = unsafeGetAt img (w * y + x)


{-# INLINE unsafeGetAt #-}
unsafeGetAt :: Image w h p -> Int -> p
unsafeGetAt img@(UnsafeImage _ _ ps) i = ps V.! i


makeImageWithSing_ :: Sing w -> Sing h -> V.Vector p -> Image w h p
makeImageWithSing_ dx dy = 
    let
        -- pw = (Proxy :: Proxy w)
        -- ph = (Proxy :: Proxy h)
        w' = fromIntegral (fromSing dx)
        h' = fromIntegral (fromSing dy)
    in UnsafeImage w' h'


fromUnsafeImage_
  :: UnsafeBoxedImage p
  -> (forall w h. (SingI w, SingI h) => Image w h p -> k)
  -> k
fromUnsafeImage_ img@(UnsafeBoxedImage x y ps) f =
    let xVal = toSing (fromIntegral x)
    in case (xVal :: SomeSing TN.Nat) of
        SomeSing (xVal :: Sing w) ->
            withSingI xVal $
                let yVal = toSing (fromIntegral y)
                in case (yVal :: SomeSing TN.Nat) of
                    SomeSing (yVal :: Sing h) ->
                        withSingI yVal $
                            let safeImg = makeImageWithSing_ xVal yVal ps
                            in f safeImg


fromUnsafeImage :: UnsafeBoxedImage p -> Image w h p
fromUnsafeImage img = fromUnsafeImage_ img (\(UnsafeImage w' h' ps) -> UnsafeImage w' h' ps)


data UnsafeBoxedImage p = UnsafeBoxedImage {
    imgWidth :: !Int
  , imgHeight :: !Int
  , imgPixels :: !(V.Vector p)
}


makeUnsafeBoxedImage :: Int -> Int -> (Int -> Int -> p) -> UnsafeBoxedImage p
makeUnsafeBoxedImage w h pf =
    let pxs = [pf x y | y <- [0..h-1], x <- [0..w-1]]
        ps = V.fromList pxs
    in UnsafeBoxedImage w h ps
