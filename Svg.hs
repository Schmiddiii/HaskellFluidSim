
module Svg where
    
import Settings
import Types
import Fluid

-- |Surrounds a String with the svg header and trail.
surroundWithHeader :: String -> String
surroundWithHeader str = "<?xml version='1.0' encoding='UTF-8'?> <svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' version='1.1' baseProfile='full'>" ++ str ++ "</svg>"


-- |Converts a color to 'rgb(r,g,b)'.
colorToRgb :: Color -> String
colorToRgb (r,g,b) = "rgb("++(show r)++","++(show g)++","++(show b)++")"

-- |Creates a Rectangle
mkRect :: (Float, Float) -> -- ^The coordinates of the rectangle, x then y
          (Float, Float) -> -- ^The size of the rectangle, width then height
          Color -> -- ^The color of the rectangle
          String -> -- ^The svg in the the rectangle, e.g. animations
          String
mkRect (x,y) (w,h) c str = "<rect x='"++(show x)++"' y='"++(show y)++"' width='"++(show w)++"' height='"++(show h)++"' style='fill:"++(colorToRgb c)++"' >" ++
                          str ++
                          "</rect>"

-- |Creates a Animation
mkAnim :: String -> -- ^The attribute name
          Float ->    -- ^The beginning time in seconds
          Float ->    -- ^The duration in seconds
          [Float] ->  -- ^The values
          String
mkAnim attName beg dur val = "<animate attributeName='"++attName++
                             "' begin='"++(show beg)++
                             "s' dur='"++(show dur)++
                             "s' values='"++concat [(show v)++";" | v<-val]++"' />"


drawFluid :: (Float, Float) -> -- ^Size of a cell, width then height
            Float ->          -- ^Duration in seconds
            FluidMatrixTime Fluid Wall ->  -- ^Fluid matrix to animate
            String
drawFluid _ _ (FluidMatrixTime [] _ _ ) = ""
drawFluid (w, h) dur (FluidMatrixTime state fluid wall) =
        concat [
          rec row col (getCol row col)
          (mkAnim "opacity" 0 dur (getPre(extractAt state row col)))
        | col <- [0..((length (state!!0!!0))-1)], row <- [0..((length (state!!0))-1)] ]
        where rec row col = mkRect (fromIntegral(col)*w, fromIntegral(row)*h) (w,h)
--              extractAt state row col = [((state!!time)!!row)!!col | time <-[0..((length state)-1)]]
              getPre = map (\x -> x/maxPressure) . map getPressure
              getCol row col = case state!!0!!row!!col of
                                   FluidElem _ -> getFluidColor fluid
                                   WallElem -> getWallColor wall

extractAt :: [FluidMatrix] -> Int -> Int -> [GridElem]
extractAt [] _ _ = []
extractAt (x:xs) row col = (x!!row!!col) : (extractAt xs row col)


