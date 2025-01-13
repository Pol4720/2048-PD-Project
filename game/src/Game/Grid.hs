module Game.Grid (
    -- Lista de exportación
    Grid,
    emptyGrid,
    placeTile,
    moveTiles,
    updateGrid
) where

import Game.Tile (Tile(..))  -- Importar la definición de Tile desde Tile.hs

-- Definimos el tipo de dato Grid como una lista de listas de Tiles
type Grid = [[Tile]]

-- Tamaño del tablero
gridSize :: Int
gridSize = 4

-- Inicializa un tablero vacío
emptyGrid :: Grid
emptyGrid = replicate gridSize (replicate gridSize Empty)

-- Actualiza el valor de una celda en el tablero
updateGrid :: Grid -> Int -> Int -> Tile -> Grid
updateGrid grid x y val =
    take x grid ++ [take y (grid !! x) ++ [val] ++ drop (y + 1) (grid !! x)] ++ drop (x + 1) grid

-- Verifica si una celda está vacía
isEmpty :: Tile -> Bool
isEmpty Empty = True
isEmpty _     = False

-- Definir la función placeTile
placeTile :: Grid -> (Int, Int) -> Tile -> Grid
placeTile grid (x, y) tile = updateGrid grid x y tile

-- Definir la función moveTiles
moveTiles :: Grid -> Grid
moveTiles grid = grid -- Implementación de ejemplo, debe ser reemplazada por la lógica de movimiento real
