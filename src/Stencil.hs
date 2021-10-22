{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Stencil where

import Control.Applicative
import GHC.TypeLits

import Internal


data Padding e = Padding
    { paddingFromOrigin :: Coord
    , paddingFromBottom :: Coord
    , paddingElement :: e
    }




data Stencil e a = Stencil
    { stencilSize :: Dims -- stencil size
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



instance Num a => Num (Stencil e a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}



instance Fractional a => Fractional (Stencil e a) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  recip = fmap recip
  {-# INLINE recip #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}



instance Floating a => Floating (Stencil e a) where
  pi = pure pi
  {-# INLINE pi #-}
  exp = fmap exp
  {-# INLINE exp #-}
  log = fmap log
  {-# INLINE log #-}
  sqrt = fmap sqrt
  {-# INLINE sqrt #-}
  (**) = liftA2 (**)
  {-# INLINE (**) #-}
  logBase = liftA2 logBase
  {-# INLINE logBase #-}
  sin = fmap sin
  {-# INLINE sin #-}
  cos = fmap cos
  {-# INLINE cos #-}
  tan = fmap tan
  {-# INLINE tan #-}
  asin = fmap asin
  {-# INLINE asin #-}
  acos = fmap acos
  {-# INLINE acos #-}
  atan = fmap atan
  {-# INLINE atan #-}
  sinh = fmap sinh
  {-# INLINE sinh #-}
  cosh = fmap cosh
  {-# INLINE cosh #-}
  tanh = fmap tanh
  {-# INLINE tanh #-}
  asinh = fmap asinh
  {-# INLINE asinh #-}
  acosh = fmap acosh
  {-# INLINE acosh #-}
  atanh = fmap atanh
  {-# INLINE atanh #-}


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



makeMaxFilterStencil :: (RealFrac r, Ord e) 
                     => r -- filter radius
                     -> e -- black value
                     -> Stencil e e
makeMaxFilterStencil r blackVal =
    let radius = adjustRadius r
        r2 = floor (radius * radius) + 1
        kernelRadius = isqrt (radius * radius + 1)
        adjustRadius :: RealFrac a => a -> a
        adjustRadius r
            | r >= 1.5 && r < 1.75 = 1.75
            | r >= 2.5 && r < 2.85 = 2.85
            | otherwise = r
        isqrt :: RealFrac a => a -> Int
        isqrt v = floor $ sqrt (realToFrac v + 1e-10)
        -- kernel coords
        kcoords = concatMap (\i ->
            if i == 0 then
                [ (x, 0) | x <- [-kernelRadius..kernelRadius] ]
            else
                let dx = isqrt (fromIntegral (r2 - i * i))
                in
                    [ (x, y) | y <- [-i, i], x <- [-dx..dx] ]
            )
            [0..kernelRadius]
        -- kernel center
        kcenter = (kernelRadius, kernelRadius)
        -- kernel size
        ksize = (2*kernelRadius+1, 2*kernelRadius+1)
        -- stencil function
        sf uget _ ix =
            foldr accum blackVal kcoords
            where
                -- acum :: a -> b -> b
                accum kIx = max (uget (liftCoord2 (-) ixOff kIx))
                ixOff = liftCoord2 (+) ix kcenter

    in
        Stencil ksize kcenter sf



-- applyStencil :: Stencil e a
--              -> Padding e
--              -> Image w h e
--              -> Image w h a
-- applyStencil st padding img =



-- applyStencilToWindow :: Stencil e a
--                      -> Window e
--                      -> Window a
-- applyStencilToWindow stencil@(Stencil sz, c, sf) w@(Window st sz wi) =
--     -- sf :: (Coord -> e) -> (Coord -> e) -> Coord -> a
--     -- wi :: Coord -> e
--     ImageWindow st sz sf 