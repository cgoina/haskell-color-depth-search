module ArrayImage (
    ArrayImage
  , readArrayImage
  , writeArrayImageAsPng
  , clearRegion
  , horizontalMirror
  , maxFilter
) where

import Control.Applicative ( Applicative(liftA2) )
import qualified Codec.Picture as JP
import qualified Data.Massiv.Array as A


newtype ArrayImage p = ArrayImage {
    pixels :: A.Array A.D A.Ix2 p
}


instance Functor ArrayImage where
    fmap f img@(ArrayImage ps) = ArrayImage $ fmap f ps


instance Applicative ArrayImage where
    pure p = ArrayImage $ A.singleton p
    ffimg@(ArrayImage pff) <*> faimg@(ArrayImage pfa) = ArrayImage $ pff <*> pfa


instance Num a => Num (ArrayImage a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger i = pure $ fromInteger i


width :: ArrayImage p -> Int
width img@(ArrayImage ps) = 
    let A.Sz2 w _ = A.size ps
    in w


height :: ArrayImage p -> Int
height img@(ArrayImage ps) = 
    let A.Sz2 _ h = A.size ps
    in h


pixelAt :: ArrayImage p -> Int -> Int -> p
pixelAt (ArrayImage ps) x y = A.evaluate' ps (x A.:. y)

readArrayImage :: FilePath -> IO (Either String (ArrayImage JP.PixelRGB8))
readArrayImage fp = do
    eimg <- JP.readImage fp
    case eimg of
        Left err -> return (Left $ "Could not read image: " ++ err)
        Right dimg ->
            return (Right (ArrayImage arr))
            where img = JP.convertRGB8 dimg
                  (w, h) = (JP.imageWidth img, JP.imageHeight img)
                  arr = A.makeArray A.Par (A.Sz2 w h) pf
                  pf (x A.:. y) = JP.pixelAt img x y


writeArrayImageAsPng :: FilePath -> ArrayImage JP.PixelRGB8 -> IO ()
writeArrayImageAsPng filePath img@(ArrayImage ps) = JP.writePng filePath $
    JP.generateImage (pixelAt img) (width img) (height img)


horizontalMirror :: ArrayImage p -> ArrayImage p
horizontalMirror img@(ArrayImage ps) = ArrayImage $ A.imap f ps
    where
        f ix _ = let (x A.:. y) = ix
                 in pixelAt img (width img - x - 1) y


class (JP.Pixel p) => BackgroundPixel p where
    clear :: p -> p


instance BackgroundPixel JP.PixelRGB8 where
    clear p = JP.colorMap (const (0 :: JP.Pixel8)) p


clearRegion :: (BackgroundPixel p) => ArrayImage p -> (Int -> Int -> Bool) -> ArrayImage p
clearRegion img@(ArrayImage ps) pred = ArrayImage $ A.imap f ps
    where
        f ix p = let (x A.:. y) = ix
                 in
                    if pred x y then clear p
                    else p


maxFilter :: (Ord p, RealFrac r) => r -> ArrayImage p -> ArrayImage p
maxFilter r img@(ArrayImage ps) = ArrayImage $ A.imap f ps
    where
        f ix p = let (x A.:. y) = ix
                     neighbors = map (uncurry (pixelAt img)) (neighborCoords (x, y) r (width img, height img))
                in foldr1 (\x' y' -> if x' >= y' then x' else y') neighbors


neighborCoords :: (RealFrac r) => (Int, Int) -> r -> (Int, Int) -> [(Int, Int)]
neighborCoords coord r sz@(w,h) = 
    filter checkBoundaries $ map (coord +) (expandAllCoords $ circleRadii r)
    where
        checkBoundaries (x,y) = x >= 0 && x < w && y >= 0 && y < h


type RelCoord = (Int, Int)

instance (Num a, Num b) => Num (a, b) where
    (x, y) + (x', y') = (x+x', y+y')
    {-# INLINE [~1] (+) #-}
    (x, y) - (x', y') = (x-x', y-y')
    {-# INLINE [~1] (-) #-}
    (x, y) * (x', y') = (x*x', y*y')
    {-# INLINE [~1] (*) #-}
    abs (x,y) = (abs x, abs y)
    {-# INLINE [~1] abs #-}
    signum (x, y) = (signum x, signum y)
    {-# INLINE [~1] signum #-}
    fromInteger x = (fromInteger x, fromInteger x)
    {-# INLINE [~1] fromInteger #-}


circleRadii :: RealFrac r => r -> [RelCoord]
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


expandCoords :: RelCoord -> [RelCoord]
expandCoords (dx, dy) =
    if dy == 0 then
        [(dx, dy)]
    else
        [(dx, dy') | dy' <- [-dy..dy] ]


expandAllCoords :: [RelCoord] -> [RelCoord]
expandAllCoords = concatMap expandCoords
