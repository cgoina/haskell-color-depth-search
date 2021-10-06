{-# LANGUAGE ConstraintKinds #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}


module Dims ( Ix(..), Dims
            , Dim(..)
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
                                    , type (-)
                                    , type (<=?) )

import qualified GHC.TypeNats as TN ( someNatVal
                                    , type (<=) )

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


infixr 5 :>


class ( Eq dx
      , Ord dx
      , Show dx
      ) => Dimx dx where
    pureDim :: Int -> dx
    liftDim :: (Int -> Int) -> dx -> dx
    liftDim2 :: (Int -> Int -> Int) -> dx -> dx -> dx


data Dim0 = Dim0 deriving (Eq, Ord, Show)
type Dim1 = Int
data DimN (n :: TL.Nat) = Int :> (Dim (n TL.- 1))

type family Dim (n :: TL.Nat) = r | r -> n where
  Dim 0 = Dim0
  Dim 1 = Dim1
  Dim n = DimN n


instance Show (DimN 2) where
  showsPrec n (i :> j) = showsPrecWrapped n (shows i . (" :> " ++) . shows j)


instance {-# OVERLAPPABLE #-} Show (Dim (n TL.- 1)) => Show (DimN n) where
  showsPrec n (i :> ix) = showsPrecWrapped n (shows i . (" :> " ++) . shows ix)


showsPrecWrapped :: Int -> ShowS -> ShowS
showsPrecWrapped n inner
  | n < 1 = inner
  | otherwise = ('(':) . inner . (")" ++)


instance {-# OVERLAPPING #-} Eq (DimN 2) where
  (i1 :> j1) == (i2 :> j2) = i1 == i2 && j1 == j2


instance Eq (Dim (n TL.- 1)) => Eq (DimN n) where
  (i1 :> ix1) == (i2 :> ix2) = i1 == i2 && ix1 == ix2


instance {-# OVERLAPPING #-} Ord (DimN 2) where
  compare (i1 :> j1) (i2 :> j2) = compare i1 i2 <> compare j1 j2


instance Ord (Dim (n TL.- 1)) => Ord (DimN n) where
  compare (i1 :> ix1) (i2 :> ix2) = compare i1 i2 <> compare ix1 ix2


instance Dimx Dim1 where
    pureDim = id
    liftDim f = f
    liftDim2 f = f


instance Dimx (DimN 2) where
    pureDim d = d :> d
    liftDim f (d2 :> d1)= f d2 :> f d1
    liftDim2 f (d2 :> d1) (d2' :> d1') = f d2 d2' :> f d1 d1'


type HigherDimx n = ( 2 TN.<= n
                    , TL.KnownNat n
                    , TL.KnownNat (n TL.- 1)
                    , Dimx (DimN (n TL.- 1))
                    , DimN (n TL.- 1) ~ Dim (n TL.- 1))


instance {-# OVERLAPPABLE #-} HigherDimx n => Dimx (DimN n) where
    pureDim d = d :> (pureDim d :: Dim (n TL.- 1))
    liftDim f (i :> ix) = f i :> liftDim f ix
    liftDim2 f (i :> ix) (i' :> ix') = f i i' :> liftDim2 f ix ix'


instance Num (DimN 2) where
    (+) = liftDim2 (+)
    (-) = liftDim2 (-)
    (*) = liftDim2 (*)
    abs = liftDim abs
    signum = liftDim signum
    fromInteger = pureDim . fromInteger


instance {-# OVERLAPPABLE #-} HigherDimx n => Num (DimN n) where
    (+) = liftDim2 (+)
    (-) = liftDim2 (-)
    (*) = liftDim2 (*)
    abs = liftDim abs
    signum = liftDim signum
    fromInteger = pureDim . fromInteger


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
