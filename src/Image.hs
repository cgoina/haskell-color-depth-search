{-# LANGUAGE MultiWayIf #-}

module Image (
    getImage,
    makeLineRadii
) where

import Codec.Picture
import Options.Applicative.Types (OptVisibility(Internal))

-- import qualified Data.Vector as V

-- data Image a = Image {
--     width :: {-# UNPACK #-} !Int
--   , height :: {-# UNPACK #-} !Int
--   , pixels :: V.Vector a
-- }

getImage :: FilePath -> IO (Either String (Image PixelRGB8))
getImage fp = do 
    eimg <- readImage fp
    case eimg of
        Left err -> return (Left $ "Could not read image: " ++ err)
        Right img -> return (Right (convertRGB8 img))


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
        (kernelSize,
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

adjustRadius :: Float -> Float
adjustRadius r
    | r >= 1.5 && r < 1.75 = 1.75
    | r >= 2.5 && r < 2.85 = 2.85
    | otherwise = r

isqrt :: Float -> Int
isqrt v = floor $ sqrt (v + 1e-10)
