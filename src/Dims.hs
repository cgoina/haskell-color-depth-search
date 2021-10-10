{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
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


module Dims ( Dim(..)
            , Dimx(..)
            , LowerUnconsIsWf
            , ReflectedDim(..)
            , reflectDim
            ) where

import Control.Newtype
import Data.Constraint
import Data.Constraint.Unsafe
import Data.Kind ( Type )
import Data.Proxy ( Proxy(..) )
import Data.Reflection
import Data.Typeable
import qualified GHC.TypeLits as TL ( KnownNat
                                    , Nat
                                    , type (-) )

import qualified GHC.TypeNats as TN ( type (<=) )
import Unsafe.Coerce


infixr 5 :>


type family LowerRank dx :: Type


class ( Eq dx
      , Ord dx
      , Show dx
      , Eq (LowerRank dx)
      , Ord (LowerRank dx)
      , Show (LowerRank dx)
      , Typeable dx
      , TL.KnownNat (Dimensions dx)
      ) => Dimx dx where
    type Dimensions dx :: TL.Nat
    -- | Prepend a dimension to the index
    consDim :: Int -> LowerRank dx -> dx

    -- | Take a dimension from the index from the outside
    unconsDim :: dx -> (Int, LowerRank dx)

    -- | Apppend a dimension to the index
    snocDim :: LowerRank dx -> Int -> dx

    -- | Take a dimension from the index from the inside
    unsnocDim :: dx -> (LowerRank dx, Int)

    pureDim :: Int -> dx
    liftDim :: (Int -> Int) -> dx -> dx
    liftDim2 :: (Int -> Int -> Int) -> dx -> dx -> dx
    totalSize :: dx -> Int


data Dim0 = Dim0 deriving (Eq, Ord, Show)
type Dim1 = Int
data DimN (n :: TL.Nat) = Int :> (Dim (n TL.- 1))


type family Dim (n :: TL.Nat) = r | r -> n where
  Dim 0 = Dim0
  Dim 1 = Dim1
  Dim n = DimN n


instance {-# OVERLAPPING #-} Show (DimN 2) where
  showsPrec n (i :> j) = showsPrecWrapped n (shows i . (" :> " ++) . shows j)


instance Show (Dim (n TL.- 1)) => Show (DimN n) where
  showsPrec n (i :> ix) = showsPrecWrapped n (shows i . (" :> " ++) . shows ix)


showsPrecWrapped :: Int -> ShowS -> ShowS
showsPrecWrapped n inner
  | n < 1 = inner
  | otherwise = ('(':) . inner . (")" ++)


instance Eq (DimN 2) where
  (i1 :> j1) == (i2 :> j2) = i1 == i2 && j1 == j2


instance {-# OVERLAPPABLE #-} Eq (Dim (n TL.- 1)) => Eq (DimN n) where
  (i1 :> ix1) == (i2 :> ix2) = i1 == i2 && ix1 == ix2


instance Ord (DimN 2) where
  compare (i1 :> j1) (i2 :> j2) = compare i1 i2 <> compare j1 j2


instance {-# OVERLAPPABLE #-} Ord (Dim (n TL.- 1)) => Ord (DimN n) where
  compare (i1 :> ix1) (i2 :> ix2) = compare i1 i2 <> compare ix1 ix2


type instance LowerRank Int = Dim0


type instance LowerRank (DimN n) = Dim (n TL.- 1)


instance Dimx Int where
    type Dimensions Int = 1
    consDim i1 _ = i1
    unconsDim i1 = (i1, Dim0)
    snocDim _ i1 = i1
    unsnocDim i1 = (Dim0, i1)
    pureDim = id
    liftDim f = f
    liftDim2 f = f
    totalSize d = d


instance Dimx (DimN 2) where
    type Dimensions (DimN 2) = 2
    consDim i2 i1 = i2 :> i1
    unconsDim (i2 :> i1) = (i2, i1)
    snocDim i2 i1 = i2 :> i1
    unsnocDim (i2 :> i1) = (i2, i1)
    pureDim d = d :> d
    liftDim f (d2 :> d1)= f d2 :> f d1
    liftDim2 f (d2 :> d1) (d2' :> d1') = f d2 d2' :> f d1 d1'
    totalSize (d2 :> d1) = d2 * d1


type HigherDimx n = ( 3 TN.<= n
                    , TL.KnownNat n
                    , TL.KnownNat (n TL.- 1)
                    , Dimx (DimN (n TL.- 1))
                    , DimN (n TL.- 1) ~ Dim (n TL.- 1))


instance {-# OVERLAPPABLE #-} HigherDimx n => Dimx (DimN n) where
    type Dimensions (DimN n) = n
    consDim = (:>)
    unconsDim (i :> dxl) = (i, dxl)
    snocDim (i :> dxl) i1 = i :> snocDim dxl i1
    unsnocDim (i :> dxl) =
        case unsnocDim dxl of
            (dx, i1) -> (i :> dx, i1)
    pureDim d = d :> (pureDim d :: Dim (n TL.- 1))
    liftDim f (i :> ix) = f i :> liftDim f ix
    liftDim2 f (i :> ix) (i' :> ix') = f i i' :> liftDim2 f ix ix'
    totalSize (i :> ix) = i * totalSize ix


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


class (Dimx dx, Dimx (LowerRank dx)) => LowerUnconsIsWf dx


instance LowerUnconsIsWf (DimN 2)


instance HigherDimx n => LowerUnconsIsWf (DimN n)


newtype ReflectedDim a s = ReflectDim { unDim :: a } deriving (Eq, Ord, Show, Typeable)


-- data ReifiedDim dx = ReifiedDim


-- dimVal :: forall dx s. Dimx dx => ReifiedDim dx -> ReflectedDim dx s
-- dimVal dx = ReflectDim dx


type instance LowerRank (ReflectedDim dx s) = ReflectedDim (LowerRank dx) s


instance forall s dx. (Dimx dx, Typeable s) => Dimx (ReflectedDim dx s) where
    type Dimensions (ReflectedDim dx s) = Dimensions dx
    consDim i2 i1 = ReflectDim $ consDim i2 (unDim i1)
    unconsDim i2 = 
            let ui2 = unconsDim $ unDim i2
            in (fst ui2, ReflectDim (snd ui2))
    snocDim i2 i1 = ReflectDim $ snocDim (unDim i2) i1
    unsnocDim i2 = 
            let ui2 = unsnocDim $ unDim i2
            in (ReflectDim (fst ui2), snd ui2)
    pureDim i = ReflectDim $ pureDim @dx i
    liftDim f dx = ReflectDim $ liftDim f (unDim dx)
    liftDim2 f dx1 dx2 = ReflectDim $ liftDim2 f (unDim dx1) (unDim dx2)
    totalSize dx = totalSize (unDim dx)


reflectDim :: Proxy s -> a -> ReflectedDim a s
reflectDim _ a = ReflectDim a


unreflectDim :: ReflectedDim a s -> a
unreflectDim (ReflectDim a) = a



-- newtype MagicDim r = MagicDim (forall dx. Dimx dx => Proxy dx -> r)

-- reifyDim1 :: forall r. Int -> (forall dx. Dimx dx => Proxy dx -> r) -> r
-- reifyDim1 i1 k = unsafeCoerce (MagicDim k :: MagicDim r)

-- wrapDim :: dx -> ReflectedDim dx s
-- wrapDim dx = ReflectedDim dx


-- newtype Lift (p :: * -> Constraint) (a :: *) (s :: *) = Lift { lower :: a }

-- class ReifiableConstraint p where
--   data Def (p :: * -> Constraint) (a :: *)
--   reifiedIns :: Reifies s (Def p a) :- p (Lift p a s)

-- instance Newtype (Lift p a s) a where
--   pack = Lift
--   unpack = lower


-- with :: Def p a -> (forall s. Reifies s (Def p a) => Lift p a s) -> a
-- with d v = reify d $ lower . asProxyOf v


-- reifyInstance :: Def p a -> (forall (s :: *). Reifies s (Def p a) => Proxy s -> r) -> r
-- reifyInstance = reify


-- asProxyOf :: f s -> Proxy s -> f s
-- asProxyOf a _ = a


-- using :: forall p a. ReifiableConstraint p => Def p a -> (p a => a) -> a
-- using d m = reify d $ \(_ :: Proxy s) -> m \\ trans (unsafeCoerceConstraint :: (p (Lift p a s) :- p a)) reifiedIns


-- usingT :: forall p f a. ReifiableConstraint p => Def p a -> (p a => f a) -> f a
-- usingT d m = reify d $ \(_ :: Proxy s) -> m \\ trans (unsafeCoerceConstraint :: (p (Lift p a s) :- p a)) reifiedIns


-- instance ReifiableConstraint Dimx where
--   data Def Dimx a = DimxN_ { unDimx :: a }
--   reifiedIns = Sub Dict


-- instance Reifies s (Def Dimx dx) => Dimx (Lift Dimx dx s) where
--     type Dimensions (DimN_ dx s) = Dimensions dx
--     consDim i2 i1 = DimxN_ $ consDim i2 (unDim i1)
--     unconsDim i2 = 
--             let ui2 = unconsDim $ unDim i2
--             in (fst ui2, DimN_ (snd ui2))
--     snocDim i2 i1 = DimN_ $ snocDim (unDim i2) i1
--     unsnocDim i2 = 
--             let ui2 = unsnocDim $ unDim i2
--             in (DimN_ (fst ui2), snd ui2)
--     pureDim i = DimN_ $ pureDim @dx i
--     liftDim f dx = DimN_ $ liftDim f (unDim dx)
--     liftDim2 f dx1 dx2 = DimN_ $ liftDim2 f (unDim dx1) (unDim dx2)
--     totalSize dx = totalSize (unDim dx)




pattern Dim2 :: Int -> Int -> Dim 2
pattern Dim2 i2 i1 = i2 :> i1

type Dim2 = Dim 2
type Dim2T = (Int, Int)


fromDim2 :: Dim 2 -> Dim2T
fromDim2 (d2 :> d1) = (d2, d1)


toDim2 :: Int -> Int -> Dim 2
toDim2 d2 d1 = d2 :> d1
