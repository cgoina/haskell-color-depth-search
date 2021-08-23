module Image (
    Image(..)
  , readImage
  , writeImageAsPng
  , horizontalMirror
  , clearRegion
  , maxFilter
  , neighborCoords
) where


import Control.Applicative ( Applicative(liftA2) )
import qualified Codec.Picture as JP
import qualified Data.Vector as V


data Image p = Image {
    -- | image dimensions
    dims :: !(Int, Int)
    -- | pixel getter
  , pixels :: V.Vector p
}

width :: Image p -> Int
width img@(Image sz@(w, _) _) = w


height :: Image p -> Int
height img@(Image sz@(_, h) _) = h


pixelAt :: Image p -> Int -> Int -> p
pixelAt (Image sz@(w, _) ps) x y = ps V.! (y * w + x)


instance Functor Image where
    fmap f img@(Image sz ps) = Image sz $ fmap f ps


instance Applicative Image where
    pure p = Image (1, 1) $ V.singleton p
    ffimg@(Image szff pff) <*> faimg@(Image szfa pfa)
        | szff /= szfa = error "Cannot apply images of unequal dimensions."
        | otherwise = Image szfa (V.imap func pfa)
            where
                func i = pff V.! i


instance Num a => Num (Image a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger i = pure $ fromInteger i


readImage :: FilePath -> IO (Either String (Image JP.PixelRGB8))
readImage fp = do
    eimg <- JP.readImage fp
    case eimg of
        Left err -> return (Left $ "Could not read image: " ++ err)
        Right dimg ->
            return (Right (Image (w, h) pixels))
            where img = JP.convertRGB8 dimg
                  (w, h) = (JP.imageWidth img, JP.imageHeight img)
                  pixels = fmap
                            (\i ->
                                let x = i `rem` w
                                    y = i `div` w
                                in JP.pixelAt img x y)
                            (V.enumFromN 0 (w * h))


writeImageAsPng :: FilePath -> Image JP.PixelRGB8 -> IO ()
writeImageAsPng filePath img@(Image _ pf) = JP.writePng filePath $
    JP.generateImage (pixelAt img) (width img) (height img)


horizontalMirror :: Image p -> Image p
horizontalMirror img@(Image sz@(w,h) ps) = Image sz $ V.imap f ps
    where
        f i _ = let (y, x) = i `divMod` w
                in pixelAt img (width img - x - 1) y


class (JP.Pixel p) => BackgroundPixel p where
    clear :: p -> p


instance BackgroundPixel JP.PixelRGB8 where
    clear p = JP.colorMap (const (0 :: JP.Pixel8)) p


clearRegion :: (BackgroundPixel p) => Image p -> (Int -> Int -> Bool) -> Image p
clearRegion img@(Image sz@(w,h) ps) pred = Image sz $ V.imap f ps
    where
        f i p = let (y, x) = i `divMod` w
                in
                    if pred x y then clear p
                    else p


maxFilter :: (Ord p, RealFrac r) => r -> Image p -> Image p
maxFilter r img@(Image sz@(w,h) ps) = Image sz $ V.imap f ps
    where
        f i p = let (y, x) = i `divMod` w
                    neighbors = map (uncurry (pixelAt img)) (neighborCoords (x, y) r sz)
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
