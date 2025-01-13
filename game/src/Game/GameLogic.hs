module Game.GameLogic (
    -- Lista de exportación
    moveTiles,
    mergeTiles
) where

import Game.Grid (Grid, updateGrid)
import Game.Tile (Tile(..))

-- Mueve los tiles en el tablero
moveTiles :: Grid -> Grid
moveTiles grid = grid -- Implementación de ejemplo, debe ser reemplazada por la lógica de movimiento real

-- Fusiona los tiles en el tablero
mergeTiles :: Grid -> Grid
mergeTiles grid = map mergeRow grid
  where
    mergeRow row = nonEmptyTiles ++ emptyTiles
      where
        nonEmptyTiles = filter (/= Empty) row
        emptyTiles = replicate (length row - length nonEmptyTiles) Empty
