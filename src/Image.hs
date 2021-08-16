{-# LANGUAGE MultiWayIf #-}


module Image (
    getImage,
    LImage,
    width,
    height,
    makeLineRadii
) where


import Control.Comonad
import qualified Codec.Picture       as JP
import qualified Data.Vector         as V


data LImage a = LImage {
    width :: !Int
  , height :: !Int
  , pixels :: V.Vector a
}


{-# INLINE pixelAt #-}
pixelAt :: LImage a -> Int -> Int -> a
pixelAt (LImage w h ps) x y = ps V.! (y * w + x)


instance Functor LImage where
    fmap f (LImage w h ps) = LImage w h (fmap f ps)


getImage :: FilePath -> IO (Either String (LImage JP.PixelRGB8))
getImage fp = do 
    eimg <- JP.readImage fp
    case eimg of
        Left err -> return (Left $ "Could not read image: " ++ err)
        Right dimg -> 
            return (Right (LImage width height pixels))
            where img = JP.convertRGB8 dimg
                  width = JP.imageWidth img
                  height = JP.imageHeight img
                  pixels = fmap 
                            (\i -> 
                                let x = i `div` width
                                    y = i `rem` width
                                in JP.pixelAt img x y)
                            (V.enumFromN 0 (width * height -1))


makeLineRadii :: Float -> (Int, [Int])
makeLineRadii r =
    let radius = adjustRadius r
        r2 = floor (radius * radius) + 1
        kernelRadius = isqrt (radius * radius + 1.0)
        kernelHeight = 2 * kernelRadius + 1
        kernelSize = 2 * kernelHeight
        yFromIndex = \i -> if
            | i < 2 * kernelRadius ->
                if even i then
                    (2 * kernelRadius - i) `div` 2
                else
                    (2 * kernelRadius -i + 1) `div` 2
            | i > 2 * kernelRadius + 1  ->
                if even i then
                    (i - 2 * kernelRadius) `div` 2
                else
                    (i - 2 * kernelRadius - 1) `div` 2
            | otherwise -> i
    in
        (kernelRadius,
         map
            (\i -> 
                let y = yFromIndex i
                    dx = isqrt (fromIntegral (r2 - y * y))
                in if
                    | i == 2 * kernelRadius -> -kernelRadius
                    | i == 2 * kernelRadius + 1 -> kernelRadius
                    | even i -> -dx
                    | otherwise -> dx
            ) 
            [0..kernelSize-1])


{-# INLINE adjustRadius #-}
adjustRadius :: Float -> Float
adjustRadius r
    | r >= 1.5 && r < 1.75 = 1.75
    | r >= 2.5 && r < 2.85 = 2.85
    | otherwise = r

{-# INLINE isqrt #-}
isqrt :: Float -> Int
isqrt v = floor $ sqrt (v + 1e-10)

data FocusedImage a = FocusedImage {
    img :: LImage a
  , currX :: !Int
  , currY :: !Int
}

instance Functor FocusedImage where
    fmap f (FocusedImage img x y) = FocusedImage (fmap f img) x y

instance Comonad FocusedImage where
    extract (FocusedImage img x y) = pixelAt img x y

    extend f (FocusedImage img@(LImage w h _) x y) = FocusedImage
        (LImage w h $ V.generate (w * h) $ \i ->
            let (y', x') = i `divMod` w
            in f (FocusedImage img x' y'))
        x y

focus :: LImage a -> FocusedImage a
focus img 
    | width img > 0 && height img > 0 = FocusedImage img 0 0
    | otherwise = error "Cannot focus on an empty image"

neighbour :: Int -> Int -> FocusedImage a -> Maybe (FocusedImage a)
neighbour dx dy (FocusedImage img x y)
    | outOfBounds = Nothing
    | otherwise   = Just (FocusedImage img x' y')
  where
    x'          = x + dx
    y'          = y + dy
    outOfBounds =
        x' < 0 || x' >= width img ||
        y' < 0 || y' >= height img