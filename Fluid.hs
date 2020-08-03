module Fluid where

import Types
import Settings

getViscosity :: Fluid -> Float
getViscosity (Fluid _ f) = f

getFluidColor :: Fluid -> Color
getFluidColor (Fluid c _) = c

getWallColor :: Wall -> Color
getWallColor (Wall c) = c

getPressure :: GridElem -> Float
getPressure (FluidElem a) = (foldl (\x y->x+y) 0 a)/(fromIntegral (length a))
getPressure (WallElem) = maxPressure

