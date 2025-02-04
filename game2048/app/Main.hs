module Main (main)
where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game
import Data.List (transpose,reverse)
import Data.List.Split (chunksOf)
import System.Random (StdGen,getStdGen,random,randoms)
import Control.Lens (set,element)

-- Definir la función principal para ejecutar el juego
main =
  do
    g <- getStdGen
    play
      (InWindow "2048 in Haskell by Richard & Mauricio"
         (410,500)
         (20,20)
      )
      (makeColor 193 177 156 255) -- background color
      fps -- simulation steps per second
      (initPositions g) -- initial world
      drawWorld -- function to convert world to picture
      handleInputEvents -- TODO add randomness
      stepWorld

fps = 20

data Tile = Tile { val :: Int, popInTime :: Float, popOutTime :: Float} deriving (Eq,Show)
type Row = [Tile]
data World = World {board :: [[Tile]], gen :: StdGen, score :: Int }

instance Eq World where
    x == y = board x == board y && score x == score y

tileIntFunction :: (Int -> Int) -> Tile -> Tile
tileIntFunction f t = t {val=f (val t)}

setVal :: Tile -> Int -> Tile
setVal t i = t {val=i}

setVals :: [Tile] -> [Int] -> [Tile]
setVals ts is = zipWith setVal ts is

setValss :: [[Tile]] -> [[Int]] -> [[Tile]]
setValss tss iss = zipWith setVals tss iss

---------------------------------------------
-- Construccion
---------------------------------------------

toInt :: Bool -> Int
toInt True = 1
toInt False = 0

makeTile :: Int -> Tile
makeTile i = Tile {val=i, popInTime=0.0, popOutTime=0.0}

-- Comienza con dos 2s
initPositions :: StdGen -> World
initPositions g = let origBoard = chunksOf 4 $ map makeTile $ replicate 16 0
                    in addTile $ addTile World {board=origBoard,gen=g,score=0 }