
import Data.Fixed
import System.Random

import Fluid
import Settings
import Simulation
import Svg
import Types

state :: FluidMatrix
-- state = randomFluidMatrix seed gridSize maxPressure

-- Wave from center with wall
state = [[
     if ((x-(div (snd gridSize) 2))^2+(y-((div (fst gridSize)) 2))^2)<100 then
         FluidElem [1,1,1,1,1,1,1,1]
     else if (x==0 || x==((snd gridSize)-1)) then
         WallElem
     else if (y==0 || y==((fst gridSize)-1)) then
         WallElem
     else
         FluidElem [0,0,0,0,0,0,0,0]

     | x<-[0..((snd gridSize)-1)]] | y<-[0..((fst gridSize)-1)]]
-- Wave against wall
{-state = [[
    if x == (snd gridSize)-1 then
        WallElem
    else if x==0 then
        FluidElem [0,0,1,0,0,0,0,0]
    else
        FluidElem [0,0,0,0,0,0,0,0]

     | x<-[0..((snd gridSize)-1)]] | y<-[0..((fst gridSize)-1)]]
-}

seed :: Int
seed = 101

fileName :: String
fileName = "test16.svg"

-- getMaxPressure :: FluidMatrix -> Float
-- getMaxPressure fm = foldl max 0 [foldl max 0 [getPressure cell | cell <- row] | row <- fm]


main = do
        let res = simulate state water wall deltaTime timeSteps
        let out = drawFluid cellSize (deltaTime * fromIntegral(timeSteps)) res
        writeFile fileName $ (surroundWithHeader out)

        putStrLn ""



randomFluidMatrix :: Int ->   -- ^The seed
                     (Int,Int) ->   -- ^The height
                     Float -> -- ^The max pressure
                     FluidMatrix
randomFluidMatrix seed (width,height) max = [ [ FluidElem (randomEightTupel (seed+x*y) max) | y<-[1..height] ] | x<-[1..width] ]

randomEightTupel :: Int -> Float -> EightTupel
randomEightTupel seed max = map (\x-> mod' x max) (take 8 (randoms (mkStdGen seed) :: [Float]))

