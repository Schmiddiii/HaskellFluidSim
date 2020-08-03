module Types where


type Color = (Int, Int, Int)


type EightTupel = [Float]



data Fluid = Fluid Color Float deriving Show
data Wall = Wall Color deriving Show

data GridElem = FluidElem EightTupel | WallElem deriving Show

type FluidMatrix = [[GridElem]]

data FluidMatrixTime f w = FluidMatrixTime [FluidMatrix] f w deriving Show


