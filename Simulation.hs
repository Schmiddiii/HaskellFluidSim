module Simulation where

import Types
import Settings
import Fluid
import NeighborMatrix

simulate :: FluidMatrix ->               -- ^The beginning state
            Fluid ->                     -- ^The fluid
            Wall ->                      -- ^The wall
            Int ->                       -- ^The number of timesteps
            FluidMatrixTime Fluid Wall
simulate state fluid wall steps = simulateRec (FluidMatrixTime [state] fluid wall) steps


simulateRec :: FluidMatrixTime Fluid Wall -> -- ^The already computed time
               Int ->                        -- ^The number of timesteps
               FluidMatrixTime Fluid Wall
simulateRec fm 0 = fm
simulateRec (FluidMatrixTime states fluid wall) step =
    simulateRec (FluidMatrixTime (states ++ [simulateOne (states!!((length states)-1)) fluid]) fluid wall) (step-1)



simulateOne :: FluidMatrix ->  -- ^The previous state
               Fluid ->        -- ^The used fluid
               FluidMatrix
simulateOne fm fluid = flow (collision fm fluid)


{-flow :: FluidMatrix -> FluidMatrix
flow fm = [[ getAt fm x y | x<-[0..((length fm) - 1)] ] | y<-[0..((length fm)-1)]]
    where getAt :: FluidMatrix -> Int -> Int -> GridElem
          getAt fm x y = case ((fm!!y)!!x) of
                             (WallElem) -> (WallElem)
                             (FluidElem vec) -> (FluidElem [ getFrom fm x y dx dy dir | (dx,dy,dir)<-[(0,-1,0),(-1,-1,1),(-1,0,2),(-1,1,3),(0,1,4),(1,1,5),(1,0,6),(1,-1,7)] ])
          getFrom :: FluidMatrix -> Int -> Int -> Int -> Int -> Int -> Float
          getFrom fm x y dx dy dir = case (getModAt (getModAt fm (y+dy)) (x+dx)) of
                                         (WallElem) -> case ((fm)!!y)!!x of
                                                           (WallElem) -> 0
                                                           (FluidElem vec) -> getNeighbor vec dir
                                         (FluidElem vec) -> getNeighbor vec (dir)
          getModAt :: [a] -> Int -> a
          getModAt vec dir = (vec!!(mod dir (length vec)))
-}

flow :: FluidMatrix -> FluidMatrix
flow fm = mapWithValueNeighbors getFrom fm
    where getFrom :: GridElem -> Neighbors GridElem -> GridElem
          getFrom (WallElem) _ = WallElem
          getFrom (FluidElem _) (n0, n1, n2, n3, n4, n5, n6, n7) =
              FluidElem (flowFrom n4 0, flowFrom n5 1, flowFrom n6 2, flowFrom n7 3, flowFrom n0 4, flowFrom n1 5, flowFrom n2 6, flowFrom n3 7)
          flowFrom :: GridElem -> Int -> Float
          flowFrom (FluidElem n) i = getNeighbor n i
          flowFrom WallElem i = 0  -- TODO



collision :: FluidMatrix ->   -- ^The fluid matrix
             Fluid ->         -- ^The fluid used
             FluidMatrix
collision fm fluid = mapMatrix collide fm
               where collide :: GridElem -> GridElem
                     collide WallElem = WallElem
--                     collide (FluidElem vec) = (FluidElem [(vec!!i) + ((getEquality (FluidElem vec) i)-(vec!!i))/(getViscosity fluid) | i<-[0..((length vec)-1)] ])
                     collide (FluidElem vec) = (FluidElem (f 0, f 1, f 2, f 3, f 4, f 5, f 6, f 7))
                         where f n = (getNeighbor vec n) + ((getEquality (FluidElem vec) n) - (getNeighbor vec n))/(getViscosity fluid)

getEquality :: GridElem -> Int -> Float
getEquality (WallElem) _ = 0
getEquality (FluidElem vec) dir = (1/2) * ((getNeighbor vec dir)-(getNeighbor vec (dir+4))) + (1/4) * ((getNeighbor vec (dir+1))+(getNeighbor vec (dir-1))-(getNeighbor vec (dir+3))-(getNeighbor vec (dir-3)))
    where getModAt :: [a] -> Int -> a
          getModAt vec dir = (vec!!(mod dir (length vec)))

mapMatrix :: (a->b) -> [[a]] -> [[b]]
mapMatrix f m = map (map f) m
