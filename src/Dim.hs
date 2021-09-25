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
import Data.Singletons.TH ( singletons
                          , SingI(sing) )
import GHC.TypeLits (Nat, KnownNat)


$(singletons [d|
    data Dims :: Nat -> Nat -> Type where
        D :: Dims r c
    |])


-- mkDims :: (forall r c. KnownNat r, KnownNat c) => r -> c -> Dims r c
-- mkDims rows cols = D (fromSing rows) (fromSing cols)

-- data SDims :: Nat -> Nat -> Type where
--     SDims :: SNat r -> SNat c -> SDims r c



-- size :: SDims r c -> Dims r c -> (r, c)
-- size sng d = 
--     let (rows, cols) = SDims rows cols
--     in (fromSing rows, fromSing cols)


-- mkDims :: (KnownNat r, KnownNat c) => r -> c -> Dims r c

-- data SDims rows cols = SDims rows cols

-- width :: cols. SNat cols => SDims rpws cols -> cols
-- width dims = 