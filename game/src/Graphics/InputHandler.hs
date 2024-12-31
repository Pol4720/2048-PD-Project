module Graphics.InputHandler where

import Graphics.Gloss.Interface.Pure.Game
import Game.Game (GameState, moveUp, moveDown, moveLeft, moveRight)

-- Maneja los eventos de entrada del usuario
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) state = moveUp state
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state = moveDown state
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state = moveLeft state
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state = moveRight state
handleInput _ state = state
