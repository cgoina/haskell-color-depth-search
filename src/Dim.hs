{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoCUSKs #-}
{-# LANGUAGE NoNamedWildCards #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -ddump-splices #-}

module Dim where

import Data.Kind ( Type )
import Data.Singletons.TH ( genSingletons )
import GHC.TypeLits (Nat)

$(genSingletons ([''Nat]))

-- $(singletons [d|
--   data Nat = Zero | Succ Nat
--   pred :: Nat -> Nat
--   pred Zero = Zero
--   pred (Succ n) = n
--   |])


data N = Z | S N


data SSNat :: N -> Type where
    SNatZero :: SSNat 'Z
    SNatSucc :: SSNat n -> SSNat ('S n)

-- data Dims (rows :: Nat) (cols :: Nat) = D

-- data SDims rows cols = SDims rows cols

-- width :: cols. SNat cols => SDims rpws cols -> cols
-- width dims = 