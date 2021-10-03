{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}


{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

module Dims ( Ix(..), Dims
            , makeDims
            , height
            , width
            ) where

import Data.Kind ( Type )
import Data.Proxy ( Proxy(..) )


import GHC.Natural

import qualified GHC.TypeLits as TL ( Nat
                                    , KnownNat
                                    , SomeNat(..)
                                    , natVal
                                    , type (+) )

import qualified GHC.TypeNats as TN ( someNatVal )


data SNat n  = TL.KnownNat n => SNat


data SomeNat__ = forall n. SomeNat__ (SNat n)


pattern SomeNat_ :: SNat n -> TL.SomeNat
pattern SomeNat_ x <- (\case TL.SomeNat (Proxy :: Proxy n) -> SomeNat__ (SNat :: SNat n) -> SomeNat__ x)
  where
    SomeNat_ (SNat :: SNat n) = TL.SomeNat (Proxy :: Proxy n)
{-# COMPLETE SomeNat_ #-}


pattern FromSNat :: SNat n -> Natural
pattern FromSNat x <- ((`withSomeNat` SomeNat_) -> SomeNat_ x)
  where
    FromSNat = fromSNat
{-# COMPLETE FromSNat #-}


fromSNat :: SNat n -> Natural
fromSNat x@SNat = fromInteger $ TL.natVal x


toSNat :: forall n. TL.KnownNat n => Natural -> SNat n
toSNat x = withSomeNat x $ const SNat


withKnownNat :: SNat n -> (TL.KnownNat n => r) -> r
withKnownNat SNat x = x


withSomeNat :: Natural -> (forall n. SNat n -> r) -> r
withSomeNat (TN.someNatVal->TL.SomeNat (Proxy :: Proxy n)) x = x (SNat :: SNat n)


data Ix :: TL.Nat -> Type where
    Ix :: Int -> Ix n
    deriving (Show, Eq, Ord)

toInt :: Ix n -> Int
toInt (Ix v) = v


fromInt :: Int -> Ix n
fromInt v = withSomeNat sn (\x -> Ix v)
    where
        sn = fromIntegral v


instance Num (Ix n) where
  (+) = liftIndex2 (+)
  (-) = liftIndex2 (-)
  (*) = liftIndex2 (*)
  abs = liftIndex abs
  signum = liftIndex signum
  fromInteger = pureIndex . fromInteger


pureIndex :: Int -> Ix n
pureIndex = fromInt


extractIndex :: Ix n -> Int
extractIndex = toInt

liftIndex :: (Int -> Int) -> Ix a -> Ix b
liftIndex f ix = pureIndex (f (toInt ix))


liftIndex2 :: (Int -> Int -> Int) -> Ix a -> Ix b -> Ix c
liftIndex2 f ixa ixb = pureIndex (f (toInt ixa) (toInt ixb))


data Dims (w::TL.Nat) (h::TL.Nat) = D (Ix w) (Ix h)
    deriving (Show, Eq, Ord)

instance Num (Dims m n) where
    (D m n) + (D m' n') = D (m+m') (n+n')
    (D m n) - (D m' n') = D (m-m') (n-n')
    (D m n) * (D m' n') = D (m*m') (n*n')
    abs (D m n) = D (abs m) (abs n)
    signum (D m n) = D (signum m) (signum n)
    fromInteger n = D (fromInteger n) (fromInteger n)


makeDims :: Int -> Int -> Dims w h
makeDims dx dy = D (pureIndex dx) (pureIndex dy)


width :: Dims w h -> Int
width (D dx _) = extractIndex dx


height :: Dims w h -> Int
height (D _ dy) = extractIndex dy
