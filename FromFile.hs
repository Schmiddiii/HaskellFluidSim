module FromFile where

import Types


fromString :: String -> FluidMatrix
fromString str = map fromLine (lines str)

fromLine :: String -> [GridElem]
fromLine str = map fromWord (words str)

fromWord :: String -> GridElem
fromWord "0" = FluidElem [0,0,0,0,0,0,0,0]
fromWord "1" = FluidElem [1,1,1,1,1,1,1,1]
fromWord "w" = WallElem

