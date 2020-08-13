
import System.Environment


import Data.Fixed

import Fluid
import FromFile
import Settings
import Simulation
import Svg
import Types

inputFile :: String
inputFile = "inputBarriers.txt"

outputFile :: String
outputFile = "outputBarriers.svg"

main = do
    file <- readFile inputFile
    let state = fromString file
    let res = simulate state water wall timeSteps
    let out = drawFluid cellSize (deltaTime * fromIntegral(timeSteps)) res
    writeFile outputFile $ (surroundWithHeader out)


