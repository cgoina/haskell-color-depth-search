{-# LANGUAGE MultiWayIf #-}


module Image (
    LImage(..)
  , clearRegion
  , horizontalMirror
  , maxFilter
  , readImage
  , writeImageAsPng
) where


import Control.Comonad ( Comonad(extract, extend), Functor )
import Data.Maybe (maybeToList)
import qualified Codec.Picture       as JP
import qualified Data.Vector         as V

data LImage p = LImage {
    width :: !Int
  , height :: !Int
  , pixelAt :: Int -> Int -> p
}


instance Functor LImage where
    fmap f (LImage w h ps) = LImage w h (\x y -> f $ ps x y)


readImage :: FilePath -> IO (Either String (LImage JP.PixelRGB8))
readImage fp = do
    eimg <- JP.readImage fp
    case eimg of
        Left err -> return (Left $ "Could not read image: " ++ err)
        Right dimg ->
            return (Right (LImage width height pixelAt))
            where img = JP.convertRGB8 dimg
                  width = JP.imageWidth img
                  height = JP.imageHeight img
                  pixelAt = JP.pixelAt img


writeImageAsPng :: FilePath -> LImage JP.PixelRGB8 -> IO ()
writeImageAsPng filePath img = JP.writePng filePath $
    JP.generateImage (pixelAt img) (width img) (height img)


clearRegion :: LImage JP.PixelRGB8 -> (Int -> Int -> Bool) -> LImage JP.PixelRGB8
clearRegion img@(LImage w h pf) pred = LImage w h cpf
    where
        setBlack :: JP.PixelBaseComponent JP.PixelRGB8 -> JP.PixelBaseComponent JP.PixelRGB8
        setBlack _ = 0
        cpf = \x y -> if pred x y then
                            JP.colorMap setBlack (pf x y)
                      else pf x y

maxFilter :: (Ord p, RealFrac r) => r -> LImage p -> LImage p
maxFilter r img@(LImage w h pf) = LImage w h mpf
    where
        mpf = \x y -> maxPixel r (FocusedImage img x y)


maxPixel :: (Ord p, RealFrac r) => r -> FocusedImage p -> p
maxPixel r pixel = max [
    extract p
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


data FocusedImage p = FocusedImage {
    img :: LImage p
  , currX :: !Int
  , currY :: !Int
}


instance Functor FocusedImage where
    fmap f (FocusedImage img x y) = FocusedImage (fmap f img) x y


instance Comonad FocusedImage where
    extract (FocusedImage img x y) = pixelAt img x y

    extend f (FocusedImage img@(LImage w h _) x y) = FocusedImage
        (LImage w h $ \x' y' -> f (FocusedImage img x' y')) x y


focus :: LImage p -> FocusedImage p
focus img
    | width img > 0 && height img > 0 = FocusedImage img 0 0
    | otherwise = error "Cannot focus on an empty image"


unfocus :: FocusedImage p -> LImage p
unfocus fimg@(FocusedImage img _ _) = img


neighbour dx dy (FocusedImage img x y)
    | outOfBounds = Nothing
    | otherwise   = Just (FocusedImage img x' y')
  where
    x'          = x + dx
    y'          = y + dy
    outOfBounds =
        x' < 0 || x' >= width img ||
        y' < 0 || y' >= height img

horizontalMirror :: LImage p -> LImage p
horizontalMirror img@(LImage w h pf) = LImage w h (\x y -> pf (w - x - 1) y)

