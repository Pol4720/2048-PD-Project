module Game.Game where

import Game.Grid (Grid, emptyGrid)
import Game.Score (Score, initialScore)

-- Define el estado del juego
data GameState = GameState
    { grid :: Grid
    , score :: Score
    }

-- Inicializa el estado del juego
initialGameState :: GameState
initialGameState = GameState
    { grid = emptyGrid
    , score = initialScore
    }

-- Renderiza el estado del juego
render :: GameState -> Picture
render = undefined -- Implementar la función de renderizado

-- Actualiza el estado del juego
update :: Float -> GameState -> GameState
update _ = id -- Implementar la lógica de actualización
