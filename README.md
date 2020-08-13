
# Usage

To use it right now you have to manually edit Main.hs and change the variables "inputFile" and "outputFile".
The input is a file in the git repository with a format described below. To run the program load Main.hs into ghci and run main.
After a few seconds the output file will be created. This is a svg file and can for example be viewed in Firefox.

# File format for input

The input file is layed out in a grid. Every element in the grid is "1", "0" or "w".
"1" means the fluid is flowing away from this point.
"0" means the fluid is not moving at this point.
"w" represents a wall.
Example files can be viewed in the Tests directory.


# More options

More options can be found in Settings.hs. The folloring options are described:

- water: Fluid with attribute color in RGB as (Float, Float, Float) (from 0-255) and viscosity as Float.
- wall: Wall with attribute color in RGB.
- cellSize: The size of each cell as (Float, Float) in the resulting animation.
- maxPressure: The pressure  as Float that is displayed with full opacity. Only usefull in the resulting animation. Lowering this will make you see more detail in the end but less in the beginning.
- deltaTime: The time in the animation between each frame.
- timesteps: How much frames the animation will have as Int.
