{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Stencil where

import GHC.TypeLits


type Coord = (Int, Int)


liftCoord :: (Int -> Int) -> Coord -> Coord
liftCoord f (i2, i1) = (f i2, f i1)
{-# INLINE [1] liftCoord #-}


liftCoord2 :: (Int -> Int -> Int) -> Coord -> Coord -> Coord
liftCoord2 f (i2, i1) (i2', i1') = (f i2 i2', f i1 i1')
{-# INLINE [1] liftCoord2 #-}


instance (Num a, Num b) => Num (a, b) where
    (x, y) + (x', y') = (x+x', y+y')
    (x, y) - (x', y') = (x-x', y-y')
    (x, y) * (x', y') = (x*x', y*y')
    abs (x,y) = (abs x, abs y)
    signum (x, y) = (signum x, signum y)
    fromInteger x = (fromInteger x, fromInteger x)


data Stencil e a = Stencil
    { stencilSize :: Coord -- stencil size
    , stencilCenter :: Coord -- stencil center coord
    , stencilFunc :: (Coord -> e) -- unsafe Get
                  -> (Coord -> e) -- safe Get
                  -> Coord -- convolved image coord
                  -> a
    }


instance Functor (Stencil e) where
  fmap = rmapStencil
  {-# INLINE fmap #-}


instance Applicative (Stencil e) where
  pure a = Stencil (1, 1) (0, 0) (\_ _ _ -> a)
  {-# INLINE pure #-}
  (<*>) s1@(Stencil _ _ f1) s2@(Stencil _ _ f2) = Stencil newSz maxCenter stF
    where
      stF ug gV ix = f1 ug gV ix (f2 ug gV ix)
      {-# INLINE stF #-}
      newSz = unionStencilSizes maxCenter s1 s2
      maxCenter = unionStencilCenters s1 s2
  {-# INLINE (<*>) #-}


dimapStencil :: (c -> d) -> (a -> b) -> Stencil d a -> Stencil c b
dimapStencil f g stencil@Stencil {stencilFunc = sf} = stencil {stencilFunc = sf'}
    where
        -- sf :: (Coord -> d) -> (Coord -> d) -> Coord
        -- sf' :: (Coord -> c) -> (Coord -> c) -> Coord
        sf' us s = g . sf (f . us) (f . s) 
        {-# INLINE sf' #-}
{-# INLINE dimapStencil #-}


lmapStencil :: (c -> d) -> Stencil d a -> Stencil c a
lmapStencil f stencil@Stencil {stencilFunc = sf} = stencil {stencilFunc = sf'}
    where
        -- sf :: (Coord -> d) -> (Coord -> d) -> Coord
        -- sf' :: (Coord -> c) -> (Coord -> c) -> Coord
        sf' us s = sf (f . us) (f . s) 
        {-# INLINE sf' #-}
{-# INLINE lmapStencil #-}


rmapStencil :: (a -> b) -> Stencil d a -> Stencil d b
rmapStencil g stencil@Stencil {stencilFunc = sf} = stencil {stencilFunc = sf'}
    where
        -- sf :: (Coord -> d) -> (Coord -> d) -> Coord
        -- sf' :: (Coord -> c) -> (Coord -> c) -> Coord
        sf' us s = g . sf us s
        {-# INLINE sf' #-}
{-# INLINE rmapStencil #-}


unionStencilCenters :: Stencil e1 a1 -> Stencil e2 a2 -> Coord
unionStencilCenters (Stencil _ sC1 _) (Stencil _ sC2 _) = liftCoord2 max sC1 sC2
{-# INLINE unionStencilCenters #-}


unionStencilSizes :: Coord -> Stencil e1 a1 -> Stencil e2 a2 -> Coord
unionStencilSizes maxCenter (Stencil sz1 sC1 _) (Stencil sz2 sC2 _) =
  liftCoord2 (+) maxCenter $ liftCoord2 max (liftCoord2 (-) sz1 sC1) (liftCoord2 (-) sz2 sC2)
{-# INLINE unionStencilSizes #-}
