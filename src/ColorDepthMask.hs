module ColorDepthMask where


import Image( Image( width)
            , regionPixelsAndCoords
            , aboveThreshold )


data ShiftOptions = None | One | Two
                    deriving Show


getXyShift :: ShiftOptions -> Int
getXyShift None = 0
getXyShift One = 1
getXyShift Two = 2


createAllColorDepthMasks :: (Image s p, Ord t, Num t)
    => s p -- query image
    -> t -- mask threshold
    -> Bool -- mirror the mask
    -> ShiftOptions -- shift options
    -> [[(Int,p)]]
createAllColorDepthMasks qImg maskThreshold mirrorFlag pixelShift =
    let w = width qImg
        xyShift = getXyShift pixelShift
        xyShiftTransforms = [\(x,y) -> (x+dx,y+dy) | dy <- [-xyShift..xyShift], dx <- [-xyShift..xyShift]]
        xyShiftMirrorTransforms = [\(x,y) -> (w-(x+dx)-1,y+dy) | dy <- [-xyShift..xyShift], dx <- [-xyShift..xyShift]]
        masksExtractor = map (flip (regionPixelsAndCoords qImg) (`aboveThreshold` maskThreshold))
        masks = masksExtractor xyShiftTransforms
        mirrorMasks = if mirrorFlag then masksExtractor xyShiftMirrorTransforms else []
    in
        masks ++ mirrorMasks
