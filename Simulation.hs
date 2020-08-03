module Simulation where

import Types
import Settings
import Fluid

simulate :: FluidMatrix ->               -- ^The beginning state
            Fluid ->                     -- ^The fluid
            Wall ->                      -- ^The wall
            Float ->                     -- ^The delta time
            Int ->                       -- ^The number of timesteps
            FluidMatrixTime Fluid Wall
simulate state fluid wall delta steps = simulateRec (FluidMatrixTime [state] fluid wall) delta steps


simulateRec :: FluidMatrixTime Fluid Wall -> -- ^The already computed time
               Float ->                      -- ^The delta time
               Int ->                        -- ^The number of timesteps
               FluidMatrixTime Fluid Wall
simulateRec fm _ 0 = fm
simulateRec (FluidMatrixTime states fluid wall) delta step =
    simulateRec (FluidMatrixTime (states ++ [simulateOne (states!!((length states)-1)) fluid delta]) fluid wall) delta (step-1)



simulateOne :: FluidMatrix ->  -- ^The previous state
               Fluid ->        -- ^The used fluid
               Float ->        -- ^The delta time
               FluidMatrix
simulateOne fm fluid _ = flow (collision fm fluid)


flow :: FluidMatrix -> FluidMatrix
flow fm = [[ getAt fm x y | x<-[0..((length fm) - 1)] ] | y<-[0..((length fm)-1)]]
    where getAt :: FluidMatrix -> Int -> Int -> GridElem
          getAt fm x y = case ((fm!!y)!!x) of
                             (WallElem) -> (WallElem)
                             (FluidElem vec) -> (FluidElem [ getFrom fm x y dx dy dir | (dx,dy,dir)<-[(0,-1,0),(-1,-1,1),(-1,0,2),(-1,1,3),(0,1,4),(1,1,5),(1,0,6),(1,-1,7)] ])
          getFrom :: FluidMatrix -> Int -> Int -> Int -> Int -> Int -> Float
          getFrom fm x y dx dy dir = case (getModAt (getModAt fm (y+dy)) (x+dx)) of
                                         (WallElem) -> case ((fm)!!y)!!x of
                                                           (WallElem) -> 0
                                                           (FluidElem vec) -> vec!!dir
                                         (FluidElem vec) -> getModAt vec (dir)
          getModAt :: [a] -> Int -> a
          getModAt vec dir = (vec!!(mod dir (length vec)))


collision :: FluidMatrix ->   -- ^The fluid matrix
             Fluid ->         -- ^The fluid used
             FluidMatrix
collision fm fluid = mapMatrix collide fm
               where collide :: GridElem -> GridElem
                     collide WallElem = WallElem
                     collide (FluidElem vec) = (FluidElem [(vec!!i) + ((getEquality (FluidElem vec) i)-(vec!!i))/(getViscosity fluid) | i<-[0..((length vec)-1)] ])

getEquality :: GridElem -> Int -> Float
getEquality (WallElem) _ = 0
getEquality (FluidElem vec) dir = (1/2) * ((getModAt vec dir)-(getModAt vec (dir+4))) + (1/4) * ((getModAt vec (dir+1))+(getModAt vec (dir-1))-(getModAt vec (dir+3))-(getModAt vec (dir-3)))
    where getModAt :: [a] -> Int -> a
          getModAt vec dir = (vec!!(mod dir (length vec)))

mapMatrix :: (a->b) -> [[a]] -> [[b]]
mapMatrix f m = map (map f) m
