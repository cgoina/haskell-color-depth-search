{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}


module ImageProcessing ( clearRegion
                       , horizontalMirror
                       , maxFilter
                       , circleRadii'
                       )
where

import Image ( Image, width, height, imap
             , unsafeGetAt , unsafePixelAt )
import Pixel ( Pixel(clear) )
import Stencil
import Internal

clearRegion :: forall w h p. Pixel p => Image w h p -> (Int -> Int -> Bool) -> Image w h p
clearRegion img regionCond = imap f img
    where f x y p = if regionCond x y then
                        clear p
                    else
                        unsafePixelAt img x y


horizontalMirror :: Image w h p -> Image w h p
horizontalMirror img = imap f img
    where
        w = width img
        f x y p = unsafePixelAt img (w - x - 1) y


shift :: forall w h p. Pixel p => Image w h p
                               -> Int -- delta x
                               -> Int -- delta y
                               -> Image w h p
shift img dx dy = imap f img
    where
        (w, h) = (width img, height img)
        f x y p =
            let shiftedX = x + dx
                shiftedY = y + dy
            in
                if shiftedX >= 0 && shiftedX < w && shiftedY >= 0 && shiftedY < h then
                    unsafePixelAt img shiftedX shiftedY
                else
                    clear p


maxFilter :: forall w h r p. (Ord p, RealFrac r) => r -> Image w h p -> Image w h p
maxFilter r img = imap f img
    where
        (w, h) = (width img, height img)
        filterCoords = neighborCoords r
        f x y p = let filterPixels = map (unsafeGetAt img) (absNeighborIndexes (x, y) (w, h) filterCoords)
                  in maximum filterPixels


absNeighborIndexes :: Coord -> Dims -> [Coord] -> [Int]
absNeighborIndexes p sz@(w, h) coords = map toIndex (filter checkBoundaries (map (p +) coords))
    where
        checkBoundaries :: Coord -> Bool
        checkBoundaries (x, y) = x >= 0 && x < w && y >= 0 && y < h
        toIndex (x, y) = y * w + x


neighborCoords :: (RealFrac r) => r -> [Coord]
neighborCoords = expandAllCoords . circleRadii'


circleRadii' :: RealFrac r => r -> [Coord]
circleRadii' r =
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
