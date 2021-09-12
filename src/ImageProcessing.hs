module ImageProcessing where

import Image ( Image(pixelAt, getAt, width, height, makeImage)
             , Pixel(clear)
             )


clearRegion :: Image s p => s p -> (Int -> Int -> Bool) -> s p
clearRegion img regionCond = makeImage (width img) (height img) (\x y -> if regionCond x y then clear (pixelAt img x y) else pixelAt img x y)


horizontalMirror :: Image s p => s p -> s p
horizontalMirror img = makeImage w (height img) (\x y -> pixelAt img (w - x - 1) y)
    where
        w = width img


type Coord = (Int, Int)

type Dims = (Int, Int)

instance (Num a, Num b) => Num (a, b) where
    (x, y) + (x', y') = (x+x', y+y')
    (x, y) - (x', y') = (x-x', y-y')
    (x, y) * (x', y') = (x*x', y*y')
    abs (x,y) = (abs x, abs y)
    signum (x, y) = (signum x, signum y)
    fromInteger x = (fromInteger x, fromInteger x)


maxFilter :: (Ord p, RealFrac r, Image s p) => r -> s p -> s p
maxFilter r img = makeImage w h pf
    where
        (w, h) = (width img, height img)
        filterCoords = neighborCoords r
        pf = \x y ->
            let filterPixels = map (getAt img) (absNeighborIndexes (x, y) (w, h) filterCoords)
            in maximum filterPixels


absNeighborIndexes :: Coord -> Dims -> [Coord] -> [Int]
absNeighborIndexes p sz@(w, h) coords = map toIndex (filter checkBoundaries (map (p +) coords))
    where
        checkBoundaries :: Coord -> Bool
        checkBoundaries (x, y) = x >= 0 && x < w && y >= 0 && y < h
        toIndex (x, y) = y * w + x


neighborCoords :: (RealFrac r) => r -> [Coord]
neighborCoords = expandAllCoords . circleRadii

circleRadii :: RealFrac r => r -> [Coord]
circleRadii r =
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
    in
        concatMap
            (\i ->
                if i == 0 then [(0, kernelRadius)] else (
                   let dx = i
                       dy = isqrt (fromIntegral (r2 - i * i))
                   in
                       [(-dx, dy), (dx, dy)])
            )
            [0..kernelRadius]


expandCoords :: Coord -> [Coord]
expandCoords (dx, dy) =
    if dy == 0 then
        [(dx, dy)]
    else
        [(dx, dy') | dy' <- [-dy..dy] ]


expandAllCoords :: [Coord] -> [Coord]
expandAllCoords = concatMap expandCoords
