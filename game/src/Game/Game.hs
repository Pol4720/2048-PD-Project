module Game.Game (
    -- Lista de exportación
    GameState,
    initialGameState,
    render,
    handleInput,
    update
) where

import Graphics.Gloss (Picture, blank) -- Importar Picture y blank desde Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event) -- Importar Event desde Graphics.Gloss.Interface.Pure.Game
import Graphics.Render (renderGame) -- Importar la función de renderizado desde Graphics.Render
import qualified Game.InputHandler as InputHandler -- Renombrar la importación de Game.InputHandler
import Game.GameState (GameState(..), initialGameState) -- Importar GameState y initialGameState

-- Definir la función render
render :: GameState -> Picture
render state = renderGame (grid state) -- Renderizar el estado del juego

-- Definir la función handleInput
handleInput :: Event -> GameState -> GameState
handleInput event state = InputHandler.handleInput event state -- Manejar la entrada del usuario

-- Definir la función update
update :: Float -> GameState -> GameState
update _ state = state -- Implementación de ejemplo, debe ser reemplazada por la lógica de actualización real
