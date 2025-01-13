module Lib
    ( initialState
    , render
    , handleInput
    , update
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (randomRIO)

-- Definir el tipo de dato Tile
data Tile = Empty | Tile { value :: Int } deriving (Eq, Show)

-- Definir el tipo de dato Grid como una lista de listas de Tiles
type Grid = [[Tile]]

-- Tamaño del tablero
gridSize :: Int
gridSize = 4

-- Definir el estado del juego
data GameState = GameState
    { grid :: Grid
    , score :: Int
    }

-- Inicializar un tablero vacío
emptyGrid :: Grid
emptyGrid = replicate gridSize (replicate gridSize Empty)

-- Inicializar el estado del juego
initialState :: GameState
initialState = GameState
    { grid = emptyGrid
    , score = 0
    }

-- Función para renderizar el estado del juego
render :: GameState -> Picture
render state = pictures $ concatMap renderRow (zip [0..] (grid state))
  where
    renderRow (y, row) = map (renderTile y) (zip [0..] row)
    renderTile y (x, tile) = translate (fromIntegral x * 100) (fromIntegral y * 100) (renderTile' tile)
    renderTile' Empty = color white $ rectangleSolid 100 100
    renderTile' (Tile n) = color (tileColor n) $ rectangleSolid 100 100
    tileColor 2 = makeColorI 238 228 218 255
    tileColor 4 = makeColorI 237 224 200 255
    tileColor _ = makeColorI 205 193 180 255

-- Función para manejar la entrada del usuario
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) state = moveUp state
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state = moveDown state
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state = moveLeft state
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state = moveRight state
handleInput _ state = state

-- Funciones de movimiento (implementación básica)
moveUp, moveDown, moveLeft, moveRight :: GameState -> GameState
moveUp state = state -- Implementación de ejemplo, debe ser reemplazada por la lógica de movimiento real
moveDown state = state -- Implementación de ejemplo, debe ser reemplazada por la lógica de movimiento real
moveLeft state = state -- Implementación de ejemplo, debe ser reemplazada por la lógica de movimiento real
moveRight state = state -- Implementación de ejemplo, debe ser reemplazada por la lógica de movimiento real

-- Función para actualizar el estado del juego
update :: Float -> GameState -> GameState
update _ state = state -- Implementación de ejemplo, debe ser reemplazada por la lógica de actualización real
