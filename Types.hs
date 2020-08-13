module Types where


type Color = (Int, Int, Int)


type EightTupel a = (a,a,a,a,a,a,a,a)

type Vector = EightTupel Float



data Fluid = Fluid Color Float deriving Show
data Wall = Wall Color deriving Show

data GridElem = FluidElem Vector | WallElem deriving Show

type FluidMatrix = [[GridElem]]

data FluidMatrixTime f w = FluidMatrixTime [FluidMatrix] f w deriving Show


getNeighbor :: EightTupel a -> Int -> a
getNeighbor (n0, n1, n2, n3, n4, n5, n6, n7) a
    | mod a 8 == 0 = n0
    | mod a 8 == 1 = n1
    | mod a 8 == 2 = n2
    | mod a 8 == 3 = n3
    | mod a 8 == 4 = n4
    | mod a 8 == 5 = n5
    | mod a 8 == 6 = n6
    | mod a 8 == 7 = n7
