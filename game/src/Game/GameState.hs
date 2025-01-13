module Game.GameState (
    -- Lista de exportación
    GameState(..),
    initialGameState,
    moveUp,
    moveDown,
    moveLeft,
    moveRight
) where

import Game.Grid (Grid, emptyGrid)
import Game.Score (Score, initialScore)

-- Definir el tipo de dato GameState
data GameState = GameState {
    grid :: Grid,
    score :: Score
}

-- Inicializar el estado del juego
initialGameState :: GameState
initialGameState = GameState {
    grid = emptyGrid,
    score = initialScore
}

-- Definir las funciones de movimiento
moveUp :: GameState -> GameState
moveUp state = state -- Implementación de ejemplo, debe ser reemplazada por la lógica de movimiento real

moveDown :: GameState -> GameState
moveDown state = state -- Implementación de ejemplo, debe ser reemplazada por la lógica de movimiento real

moveLeft :: GameState -> GameState
moveLeft state = state -- Implementación de ejemplo, debe ser reemplazada por la lógica de movimiento real

moveRight :: GameState -> GameState
moveRight state = state -- Implementación de ejemplo, debe ser reemplazada por la lógica de movimiento real
