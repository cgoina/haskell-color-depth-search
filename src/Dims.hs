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

module Dims ( Dims
            , height
            , makeDims
            , width
            ) where

import Data.Kind ( Type )
import Data.Proxy ( Proxy(..) )
import Data.Singletons ( SomeSing(..), Sing, SingI
                       , fromSing, toSing, withSingI, withSing, SingKind (fromSing))
import qualified GHC.TypeNats as TN ( Nat, KnownNat
                                     , natVal )
import qualified GHC.TypeLits.Singletons as TL ( SNat )


data Dims (w::TN.Nat) (h::TN.Nat) where
    D :: Int -> Int -> Dims w h


width :: Dims w h -> Int
width (D dx _) = dx


height :: Dims w h -> Int
height (D _ dy) = dy


withSing_ :: Sing w -> Sing h -> Dims w h
withSing_ dx dy =
    let
        w' = fromIntegral (fromSing dx)
        h' = fromIntegral (fromSing dy)
    in D w' h'


fromUnsafeDims_
  :: Int
  -> Int
  -> (forall w h. (SingI w, SingI h) => Dims w h -> k)
  -> k
fromUnsafeDims_ dx dy f =
    let dxVal = toSing (fromIntegral dx)
    in case (dxVal :: SomeSing TN.Nat) of
        SomeSing (dxVal :: Sing w) ->
            withSingI dxVal $
                let dyVal = toSing (fromIntegral dy)
                in case (dyVal :: SomeSing TN.Nat) of
                    SomeSing (dyVal :: Sing h) ->
                        withSingI dyVal $
                            let safeDims = withSing_ dxVal dyVal
                            in f safeDims


makeDims :: Int -> Int -> Dims w h
makeDims dx dy = fromUnsafeDims_ dx dy (\(D dx' dy') -> D dx' dy')
