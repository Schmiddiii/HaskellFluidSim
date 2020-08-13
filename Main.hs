
import System.Environment


import Data.Fixed

import Fluid
import FromFile
import Settings
import Simulation
import Svg
import Types

inputFile :: String
inputFile = "inputCircle.txt"

outputFile :: String
outputFile = "outputCircle.svg"

main = do
    file <- readFile inputFile
    let state = fromString file
    let res = simulate state water wall deltaTime timeSteps
    let out = drawFluid cellSize (deltaTime * fromIntegral(timeSteps)) res
    writeFile outputFile $ (surroundWithHeader out)


