module Game.InputHandler (
    -- Lista de exportación
    handleInput
) where

import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), SpecialKey(..), KeyState(..))
import Game.GameState (GameState, moveUp, moveDown, moveLeft, moveRight) -- Importar GameState y funciones de movimiento

-- Definir la función handleInput
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) state = moveUp state
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state = moveDown state
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state = moveLeft state
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state = moveRight state
handleInput _ state = state
