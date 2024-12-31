module Game.Grid where

import System.Random 
import Game.Tile (Tile(..))  -- Importamos la definición de Tile desde Tile.hs

-- Definimos el tipo de dato Grid como una lista de listas de Tiles
type Grid = [[Tile]]

-- Tamaño del tablero
gridSize :: Int
gridSize = 4

-- Inicializa un tablero vme acío
emptyGrid :: Grid
emptyGrid = replicate gridSize (replicate gridSize Empty)

-- Genera una posición aleatoria en el tablero
randomPosition :: IO (Int, Int)
randomPosition = do
    x <- randomRIO (0, gridSize - 1)
    y <- randomRIO (0, gridSize - 1)
    return (x, y)

-- Inserta un número en una posición aleatoria del tablero
insertRandomNumber :: Grid -> IO Grid
insertRandomNumber grid = do
    (x, y) <- randomPosition
    if grid !! x !! y == Empty
        then return (updateGrid grid x y (Tile 2))
        else insertRandomNumber grid

-- Actualiza el valor de una celda en el tablero
updateGrid :: Grid -> Int -> Int -> Tile -> Grid
updateGrid grid x y val =
    take x grid ++ [take y (grid !! x) ++ [val] ++ drop (y + 1) (grid !! x)] ++ drop (x + 1) grid
