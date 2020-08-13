module Settings where

import Types

water :: Fluid
water = Fluid (0,0,255) 5

wall :: Wall
wall = Wall (0,0,0)

cellSize :: (Float, Float)
cellSize = (10,10)

maxPressure :: Float
maxPressure = 0.3

deltaTime :: Float
deltaTime = 0.5

timeSteps :: Int
timeSteps = 20

