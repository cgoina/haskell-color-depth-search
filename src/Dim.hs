{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

module Dim where

import Data.Kind (Type)
import Data.Proxy
import Data.Singletons.TH ( genSingletons, singletons, withSing )

import Data.Singletons ( Sing, SingI
                       , fromSing, sing, singByProxy )

import GHC.TypeLits ( KnownNat, natVal )


$(singletons [d|
    data Dims :: Type -> Type -> Type where
        D :: Dims r c
    |])


mkDims :: SDims d -> Dims r c
mkDims _ = D

-- getDims :: (forall r c. SingI r, SingI c) => Dims r c -> r
-- getDims d = rval
--             where rval = fromSing d
            

