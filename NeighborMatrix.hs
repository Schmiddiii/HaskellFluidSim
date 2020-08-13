module NeighborMatrix where

type Matrix a = [[a]]
type Neighbors a = (a,a,a,a,a,a,a,a)


getValue :: Matrix a -> Int -> Int -> a
-- getValue m row col = getValueRec m m row col
getValue m row col = case rotate m row col of
                         ((x:_):_) -> x
                         x         -> error "Cannot get from empty list"

getNeighbors :: Matrix a -> Int -> Int -> Neighbors a
getNeighbors m row col = (at 0 1, at 0 2, at 1 2, at 2 2, at 2 1, at 2 0, at 1 0, at 0 0)
                         where at = getValue rotMat
                               rotMat = rotate m (row-1) (col-1)


rotate :: Matrix a -> Int -> Int -> Matrix a
rotate m rows cols = rotateHorizontal (rotateVertical m cols) rows

-- |n>=0 => Left rotation
rotateVertical :: Matrix a -> Int -> Matrix a
rotateVertical m n | n>=0 = iterate rotateVerticalOnceLeft m !! n
                   | otherwise = iterate rotateVerticalOnceRight m !! (-n)
                   

rotateVerticalOnceLeft :: Matrix a -> Matrix a
rotateVerticalOnceLeft m = map rotateRowOnceLeft m

rotateVerticalOnceRight :: Matrix a -> Matrix a
rotateVerticalOnceRight m = map rotateRowOnceRight m

rotateRowOnceRight :: [a] -> [a]
rotateRowOnceRight r = (last r:init r)

rotateRowOnceLeft :: [a] -> [a]
rotateRowOnceLeft [] = []
rotateRowOnceLeft (x:xs) = xs++[x]

-- | n>=0 => Up rotation
rotateHorizontal :: Matrix a -> Int -> Matrix a
rotateHorizontal m n | n>=0      = iterate rotateRowOnceLeft m !! n
                     | otherwise = iterate rotateRowOnceRight m !! (-n)

getNeighborMatrix :: Matrix a -> Matrix (Neighbors a)
getNeighborMatrix m = [ [ getNeighbors m y x | x<-[0..((length (m!!y))-1)] ] | y<-[0..((length m)-1)] ]

mapWithValueNeighbors :: (a -> Neighbors a -> b) -> Matrix a -> Matrix b
mapWithValueNeighbors f m = [ [ f (m!!y!!x) (neighborMatrix!!y!!x)| x<-[0..((length (neighborMatrix!!y)-1))]]| y<-[0..((length neighborMatrix)-1)]]
                       where neighborMatrix = getNeighborMatrix m

mapWithNeighbors :: (Neighbors a -> b) -> Matrix a -> Matrix b
mapWithNeighbors f m = [ [ f (neighborMatrix!!y!!x)| x<-[0..((length (neighborMatrix!!y)-1))]]| y<-[0..((length neighborMatrix)-1)]]
                       where neighborMatrix = getNeighborMatrix m

