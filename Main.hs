
import System.Environment


import Data.Fixed

import Fluid
import FromFile
import Settings
import Simulation
import Svg
import Types

inputFile :: String
inputFile = "Tests/inputHole.txt"

outputFile :: String
outputFile = "Tests/outputHole.svg"

main = do
    file <- readFile inputFile
    let state = fromString file
    let res = simulate state water wall timeSteps
    let out = drawFluid cellSize (deltaTime * fromIntegral(timeSteps)) res
    writeFile outputFile $ (surroundWithHeader out)


