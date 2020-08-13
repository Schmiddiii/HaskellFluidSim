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


flow :: FluidMatrix -> FluidMatrix
flow fm = mapWithValueNeighbors getFrom fm
    where getFrom :: GridElem -> Neighbors GridElem -> GridElem
          getFrom (WallElem) _ = WallElem
          getFrom (FluidElem v) n =
              FluidElem (flowFrom n4 0 n v, flowFrom n5 1 n v, flowFrom n6 2 n v, flowFrom n7 3 n v, flowFrom n0 4 n v, flowFrom n1 5 n v, flowFrom n2 6 n v, flowFrom n3 7 n v)
              where (n0, n1, n2, n3, n4, n5, n6, n7) = n
          flowFrom :: GridElem -> Int -> Neighbors GridElem -> Vector -> Float
          flowFrom (FluidElem n) i _ _ = getNeighbor n i
          flowFrom WallElem i n v | mod i 2 == 0 = getNeighbor v (i+4)                                        -- Streight into wall -> Bounce back from wall
                                  | otherwise    =  case (getNeighbor n (i-1),getNeighbor n (i+1)) of         -- Diagonal into wall
                                                        (WallElem, WallElem) -> getNeighbor v (i+4)           -- Corner -> Bounce back
                                                        (FluidElem v2, WallElem) -> getNeighbor v2 (i+2)      -- Rotationally left from wall is fluid -> check there
                                                        (WallElem, FluidElem v2) -> getNeighbor v2 (i-2)      -- Rotationally right from wall if fluid -> check there
                                                        (FluidElem v2, FluidElem v3) -> 0.5 * ((getNeighbor v2 (i+2)) + (getNeighbor v3 (i-2))) -- Fluid on both sides -> average



collision :: FluidMatrix ->   -- ^The fluid matrix
             Fluid ->         -- ^The fluid used
             FluidMatrix
collision fm fluid = mapMatrix collide fm
               where collide :: GridElem -> GridElem
                     collide WallElem = WallElem
                     collide (FluidElem vec) = (FluidElem (f 0, f 1, f 2, f 3, f 4, f 5, f 6, f 7))
                         where f n = (getNeighbor vec n) + ((getEquality (FluidElem vec) n) - (getNeighbor vec n))/(getViscosity fluid)

getEquality :: GridElem -> Int -> Float
getEquality (WallElem) _ = 0
getEquality (FluidElem vec) dir = (1/2) * ((getNeighbor vec dir)-(getNeighbor vec (dir+4))) + (1/4) * ((getNeighbor vec (dir+1))+(getNeighbor vec (dir-1))-(getNeighbor vec (dir+3))-(getNeighbor vec (dir-3)))
    where getModAt :: [a] -> Int -> a
          getModAt vec dir = (vec!!(mod dir (length vec)))

mapMatrix :: (a->b) -> [[a]] -> [[b]]
mapMatrix f m = map (map f) m
