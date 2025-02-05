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
      (InWindow "2048 en Haskell por Richard & Mauricio"
         (410,500)
         (20,20)
      )
      (makeColor 193 177 156 255) 
      fps 
      (initPositions g) 
      drawWorld
      handleInputEvents 
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
-- Construcción
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

-- cambia la animación
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
                                        translate (-300) 60 $ scale 0.2 0.2 $ color white $ text $ "Puntuación: " ++ (show s)
                                        ] ++ gameOverPicture
                                        where gameOverPicture = if lost then [gameOverMessage] else []
                                              lost = go (Just R) w == w && go (Just L) w == w 
                                                      && go (Just U) w == w && go (Just D) w == w

                                        --debugPicture ])

tileS = 90
textScale = 0.2

colorZipper :: [(Int, Color)]
colorZipper = [(2,    makeColor 238 228 218 255),
               (4,    makeColor 237 224 200 255),
               (8,    makeColor 242 177 121 255),
               (16,   makeColor 245 149  99 255),
               (32,   makeColor 246 124  95 255),
               (64,   makeColor 246 102  62 255),
               (128,  makeColor 238 208 114 255),
               (256,  makeColor 237 204  97 255),
               (512,  makeColor 237 200  80 255),
               (1024, makeColor 237 197  63 255),
               (2048, makeColor 237 194  46 255)]

getColor :: Int -> Color
getColor x = maybe white id (lookup x colorZipper)

quarterRoundedRect :: Int -> Float -> Float -> Float -> Path
quarterRoundedRect n w h r = [(0,0), (0,h/2)] ++
                              (reverse $ arcPath n (w/2-r,h/2-r) r) ++
                              [(w/2,0)]

drawQuarterRoundedRect :: Int -> Float -> Float -> Float -> Picture
drawQuarterRoundedRect n w h r = polygon $ quarterRoundedRect n w h r

outlineQuarterRoundedRect :: Int -> Float -> Float -> Float -> Picture
outlineQuarterRoundedRect n w h r = line $ quarterRoundedRect n w h r


-- toma el ancho, alto y radio y crea un rectángulo redondeado relleno
-- el entero es la precisión / número de puntos
roundedRect :: Int -> Float -> Float -> Float -> Picture
roundedRect n w h r = pictures [
                                drawQuarterRoundedRect n w h r,
                                rotate 90 $ drawQuarterRoundedRect n w h r,
                                rotate 180 $ drawQuarterRoundedRect n w h r,
                                rotate 270 $ drawQuarterRoundedRect n w h r]

-- toma x, y, r, y theta y devuelve (x+r*cos theta, y+r*sin theta)
getPoint :: Float -> Float -> Float -> Float -> (Float,Float)
getPoint x y r th = (x+r*cos th, y+r*sin th)

-- toma el centro y el radio y devuelve un arco de 90 grados con n puntos
arcPath :: Int -> (Float,Float) -> Float -> Path
arcPath n (x,y) r = map (getPoint x y r) $ [0.0] ++ (map (\x-> pi/2/(fromIntegral x)) $ reverse [1..n+1] )

tileRoundness = 4
tilePrecision = 10

-- Toma el desplazamiento en x y dibuja el fondo de la ficha
-- quizás desenrollar esto en drawTile?
tileBackColor = makeColor 205 192 180 255
drawTileBack :: Float -> Picture
drawTileBack x = color tileBackColor (translate x 0 (roundedRect tilePrecision tileS tileS tileRoundness))

-- Toma el desplazamiento en x y la ficha y dibuja la ficha misma
drawTile :: Float -> Tile -> Picture
drawTile x tile = let background = [color (getColor $ val tile) $ roundedRect tilePrecision tileS tileS tileRoundness]
                      number = if val tile > 0
                               then [translate (-20) (-10) $ scale textScale textScale $ text $ show $ val tile]
                               else []
                      curScale = if (popInTime tile) > 0
                                 then (1-(popInTime tile))
                                 else (1+(popOutTime tile))
                  in pictures
                     [ drawTileBack x,
                       translate x 0 $ scale curScale curScale $ pictures $ background ++ number]

drawRow :: Row -> Picture
drawRow [i,j,k,l] = translate (-300) 0 (pictures [ drawTile 0 i,
                                                   drawTile rowHgt j,
                                                   drawTile (rowHgt*2) k,
                                                   drawTile (rowHgt*3) l ])

--------------------------------------------
-- Manejo del tablero (movimientos y demás)--
--------------------------------------------

toWorldFunc :: ([[Tile]]->[[Tile]]) -> World -> World
toWorldFunc tf w = w {board = tf (board w)}

scootLambda :: Tile -> [Tile] -> [Tile]
scootLambda y [] = [y]
scootLambda y [x] = if val x == 0 then [x,y] else [y,x] -- TODO añadir animaciones
scootLambda y (x:xs) = if val x == 0 then x:y:xs else y:x:xs

-- Toma una fila y mueve todos los números a través de los ceros *una vez*
-- Ejemplo: [2,0,0,2] -> [0,2,0,2] y [0,2,0,2] -> [0,0,2,2] scootRowRightOnce :: [Int] -> [Int]
scootRowRightOnce = foldr scootLambda []

-- hace scootRight tres veces
scootRowRight :: [Tile] -> [Tile]
scootRowRight = scootRowRightOnce . scootRowRightOnce . scootRowRightOnce

-- mueve todo el tablero
scootRight :: [[Tile]] -> [[Tile]]
scootRight = map scootRowRight

scoot :: Maybe Direction -> [[Tile]] -> [[Tile]]
scoot Nothing = id
scoot (Just R) = scootRight
scoot (Just U) = reverse . transpose . scootRight . transpose . reverse
scoot (Just L) = transpose . reverse . transpose . scootRight . transpose . reverse . transpose
scoot (Just D) = transpose . scootRight . transpose


comboLambda :: Tile -> ([Tile],Int) -> ([Tile],Int)
comboLambda y ([],s) = ([y],s)
comboLambda y ((x:xs),s) = if val x == val y && val x > 0
                       then ((makeTile 0):(Tile {val=val x + val y, popOutTime = 0.1, popInTime = 0}):xs, s+val x+val y)
                       else (y:x:xs,s)

-- Toma una fila y hace combinaciones a la derecha en todos los números *una vez*
-- Ejemplo: [2,2,0,0] -> [0,4,0,0] y [2,2,2,2] -> [0,4,0,4]
comboRowRight :: [Tile] -> ([Tile],Int)
comboRowRight = foldr comboLambda ([],0)

-- hace combinaciones en todo el tablero
comboRight :: World -> World
-- map (comboRowRight w) (board w) => [([Tile],Int)]
comboRight w = let (newBoard, scores) = unzip $ map comboRowRight (board w)
               in w {board = newBoard, score = sum scores + score w}

combo :: Maybe Direction -> World -> World
combo Nothing = id
combo (Just R) = comboRight
combo (Just U) = (toWorldFunc $ reverse . transpose) . comboRight . (toWorldFunc $ transpose . reverse)
combo (Just L) = (toWorldFunc $ transpose . reverse . transpose) . comboRight . (toWorldFunc $ transpose . reverse . transpose)
combo (Just D) = (toWorldFunc transpose) . comboRight . (toWorldFunc transpose)

go :: Maybe Direction -> World -> World
go dir = (toWorldFunc $ scoot dir) . (combo dir) . (toWorldFunc $ scoot dir)