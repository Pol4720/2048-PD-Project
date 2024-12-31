module Graphics.Render where

import Graphics.Gloss
import Game.Grid (Grid)
import Game.Tile (Tile(..))

-- Renderiza el tablero
renderGrid :: Grid -> Picture
renderGrid grid = Pictures $ concatMap renderRow (zip [0..] grid)

-- Renderiza una fila del tablero
renderRow :: (Int, [Tile]) -> [Picture]
renderRow (y, row) = map (renderTile y) (zip [0..] row)

-- Renderiza un tile
renderTile :: Int -> (Int, Tile) -> Picture
renderTile y (x, tile) = translate (fromIntegral x * 100) (fromIntegral y * 100) (renderTile' tile)

-- Renderiza un tile específico
renderTile' :: Tile -> Picture
renderTile' Empty = color white $ rectangleSolid 100 100
renderTile' (Tile n) = color (tileColor n) $ rectangleSolid 100 100

-- Define el color de un tile basado en su valor
tileColor :: Int -> Color
tileColor 2 = makeColorI 238 228 218 255
tileColor 4 = makeColorI 237 224 200 255
tileColor _ = makeColorI 205 193 180 255
