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


replaceNthZero :: Int -> Int -> [Tile] -> [Tile]
replaceNthZero v _ [] = []
replaceNthZero v 0 (x:xs) = if val x == 0
                          then (Tile {val=v, popInTime = 0.5, popOutTime = 0.0}:xs)
                          else x:(replaceNthZero v 0 xs)
replaceNthZero v n (x:xs) = if val x == 0 then x:(replaceNthZero v (n-1) xs)
                                  else x:(replaceNthZero v n xs)

reciprocalOddsOf4 = 10

addTile :: World -> World
addTile world = let numZeros = (sum . (map toInt) . (map (==0)) . map val . concat) (board world)
                    (rand, newGen) = random (gen world) :: (Int, StdGen)
                    n = if numZeros == 0 then 0 else rand `mod` numZeros
                    (rand2, newGen2) = random newGen :: (Int, StdGen)
                    v = if rand2 `mod` reciprocalOddsOf4 == 0 then 4 else 2
                    newBoard = chunksOf 4 $ replaceNthZero v n $ concat (board world)
                in world {board=newBoard, gen=newGen}

---------------------------------------------
-- Control de direcciones
-- -----------------------------------------
--
data Direction = U | D | L | R

keyDir :: Key -> Maybe Direction
keyDir (SpecialKey KeyUp) = Just U
keyDir (SpecialKey KeyDown) = Just D
keyDir (SpecialKey KeyLeft) = Just L
keyDir (SpecialKey KeyRight) = Just R
keyDir _ = Nothing

handleInputEvents :: Event -> World -> World
handleInputEvents (EventKey k Down _ _) world = let dir = keyDir k
                                                    newWorld = go dir world
                                                in if newWorld == world
                                                   then world
                                                   else addTile newWorld
handleInputEvents  _ x = x


popInSpeed = 4
popOutSpeed = 0.5

-- cambia la animacion
updateTile :: Float -> Tile -> Tile
updateTile dt t = if val t > 0 then
                    t {popInTime = if popInTime t - dt*popInSpeed > 0 then popInTime t - dt*popInSpeed else 0,
                     popOutTime = if popOutTime t - dt*popInSpeed > 0 then popOutTime t - dt*popInSpeed else 0}
                  else t {popInTime = 0, popOutTime = 0}

updateTiles :: Float -> [[Tile]] -> [[Tile]]
updateTiles dt tss = (map (map (updateTile dt))) tss

stepWorld :: Float -> World -> World
stepWorld dt world = world {board=updateTiles dt (board world)}

-- Dibujar en pantalla

rowHgt = 100

gameOverMessage :: Picture
gameOverMessage = pictures [
                  translate (-500) (-500) $ color translucentWhite $ rectangleSolid 2000 2000,
                  translate (-335) (-150) $ scale 0.5 0.5 $ color black $ text "Game Over"
                  ]
                  where translucentWhite = makeColor 255 255 255 150

drawWorld :: World -> Picture
drawWorld w@World {board = [r1, r2, r3, r4], score=s} = translate (150) (150) $ pictures $ [ 
                                        drawRow r1,
                                        translate 0 (-rowHgt) (drawRow r2),
                                        translate 0 (-rowHgt*2) (drawRow r3),
                                        translate 0 (-rowHgt*3) (drawRow r4),
                                        translate (-300) 60 $ scale 0.2 0.2 $ color white $ text $ "Score: " ++ (show s)
                                        ] ++ gameOverPicture
                                        where gameOverPicture = if lost then [gameOverMessage] else []
                                              lost = go (Just R) w == w && go (Just L) w == w 
                                                      && go (Just U) w == w && go (Just D) w == w

                                        --debugPicture ])

tileS = 90
textScale = 0.2