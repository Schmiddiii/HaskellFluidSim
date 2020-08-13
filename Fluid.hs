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
getPressure (FluidElem (n0, n1, n2, n3, n4, n5, n6, n7)) = (n0+n1+n2+n3+n4+n5+n6+n7)/(fromIntegral 8)
getPressure (WallElem) = maxPressure


