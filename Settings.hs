module Settings where

import Types

{-universalGasConstant :: Float
universalGasConstant = 8.3144

temperature :: Float
temperature = 20
-}

water :: Fluid
water = Fluid (0,0,255) 5

wall :: Wall
wall = Wall (0,0,0)


gridSize :: (Int, Int)
gridSize = (30,30)

cellSize :: (Float, Float)
cellSize = (10,10)

maxPressure :: Float
maxPressure = 0.15

deltaTime :: Float
deltaTime = 0.5

timeSteps :: Int
timeSteps = 20

