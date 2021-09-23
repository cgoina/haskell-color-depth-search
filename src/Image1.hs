{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Image1 where

import Data.Kind (Type)
import qualified Data.Vector as V

-- import Dim ( Dims )

class Pixel p where
    type PixelComponent p :: *

    pixelComponents :: p -> [PixelComponent p]

    clear :: p -> p


data Image dims p where
    Image :: { sz :: dims
             , pixels :: !(V.Vector p) } -> Image dims p

