module Internal where

type Coord = (Int, Int)

type Dims = (Int, Int)


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


data ImageWindow p = ImageWindow
    { start :: Coord
    , size :: Dims
    , windowIndex :: Coord -> p -- pixel getter relative to the window
    }


instance Functor ImageWindow where
    fmap f imgWindow@ImageWindow { windowIndex = wi } = imgWindow { windowIndex = wi' }
        where
            wi' = f . wi


