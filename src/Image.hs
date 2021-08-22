{-# LANGUAGE FlexibleInstances #-}

module Image (
    Image(..)
  , readImage
  , writeImageAsPng
  , horizontalMirror
  , clearRegion
  , maxFilter
) where


import Control.Comonad ( Comonad(extract, extend), Functor )
import Data.Maybe (maybeToList)
import qualified Codec.Picture       as JP
import qualified Data.Vector         as V
import qualified Data.Massiv.Array   as A
import Codec.Picture (Pixel(PixelBaseComponent))

data Image p = Image {
    -- | image dimensions
    dims :: (Int, Int)
    -- | pixel getter
  , pixel :: Int -> Int -> p
}

{-# inline width #-}
width :: Image p -> Int
width img@(Image sz@(w, _) _) = w


{-# inline height #-}
height :: Image p -> Int
height img@(Image sz@(_, h) _) = h


instance Functor Image where
    fmap f img@(Image sz pf) = Image sz (\x y -> f $ pf x y)


instance Applicative Image where
    pure p = Image (1, 1) (\x y -> p)
    ffimg@(Image szff pff) <*> faimg@(Image szfa pfa)
        | szff /= szfa = error "Cannot apply images of unequal dimensions."
        | otherwise = Image szfa (\x y -> pff x y (pfa x y))


readImage :: FilePath -> IO (Either String (Image JP.PixelRGB8))
readImage fp = do
    eimg <- JP.readImage fp
    case eimg of
        Left err -> return (Left $ "Could not read image: " ++ err)
        Right dimg ->
            return (Right (Image (w, h) pixelAt))
            where img = JP.convertRGB8 dimg
                  (w, h) = (JP.imageWidth img, JP.imageHeight img)
                  pixelAt = JP.pixelAt img


writeImageAsPng :: FilePath -> Image JP.PixelRGB8 -> IO ()
writeImageAsPng filePath img@(Image _ pf) = JP.writePng filePath $ JP.generateImage pf (width img) (height img)


horizontalMirror :: Image p -> Image p
horizontalMirror img@(Image sz pf) = Image sz (\x y -> pf (width img - x - 1) y)


class (JP.Pixel p) => BlackPixel p where
    clear :: p -> p


instance BlackPixel JP.PixelRGB8 where
    clear p = JP.colorMap (const (0 :: JP.Pixel8)) p


clearRegion :: (BlackPixel p) => Image p -> (Int -> Int -> Bool) -> Image p
clearRegion img@(Image sz pf) pred = Image sz cpf
    where
        cpf = \x y -> if pred x y then clear $ pf x y
                      else pf x y

maxFilter :: (Ord p, RealFrac r) => r -> Image p -> Image p
maxFilter r img@(Image sz pf) = Image sz mpf
    where
        mpf = \x y -> maxPixel r (FocusedImage img x y)


maxPixel :: (Ord p, RealFrac r) => r -> FocusedImage p -> p
maxPixel r pixel = max [
    getFocus p
        | (dx, dy) <- expandAllCoords $ circleRadii r
        , p <- maybeToList (neighbour dx dy pixel)]
    where
        max = foldr1 (\x y -> if x >= y then x else y)


type RelCoord = (Int, Int)


circleRadii :: RealFrac r => r -> [RelCoord]
circleRadii r =
    let radius = adjustRadius r
        r2 = floor (radius * radius) + 1
        kernelRadius = isqrt (radius * radius + 1)
        {-# INLINE adjustRadius #-}
        adjustRadius :: RealFrac a => a -> a
        adjustRadius r
            | r >= 1.5 && r < 1.75 = 1.75
            | r >= 2.5 && r < 2.85 = 2.85
            | otherwise = r
        {-# INLINE isqrt #-}
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


expandCoords :: RelCoord -> [RelCoord]
expandCoords (dx, dy) =
    if dy == 0 then
        [(dx, dy)]
    else
        [(dx, dy') | dy' <- [-dy..dy] ]


expandAllCoords :: [RelCoord] -> [RelCoord]
expandAllCoords = concatMap expandCoords


neighbour :: Int -> Int -> FocusedImage p -> Maybe (FocusedImage p)
neighbour dx dy (FocusedImage img x y)
    | outOfBounds = Nothing
    | otherwise   = Just (FocusedImage img x' y')
  where
    x'          = x + dx
    y'          = y + dy
    outOfBounds =
        x' < 0 || x' >= width img ||
        y' < 0 || y' >= height img


data FocusedImage p = FocusedImage {
    img :: Image p
  , currX :: Int
  , currY :: Int
}


instance Functor FocusedImage where
    fmap f (FocusedImage img x y) = FocusedImage (fmap f img) x y


focus :: Image p -> FocusedImage p
focus img
    | width img > 0 && height img > 0 = FocusedImage img 0 0
    | otherwise = error "Cannot focus on an empty image"


getFocus :: FocusedImage p -> p
getFocus fimg@(FocusedImage img@(Image _ pf) x y) = pf x y





