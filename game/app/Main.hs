module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (randomRIO) -- Importar random
import Game.Game -- Import the main game logic

-- Define the initial state of the game
initialState :: GameState
initialState = initialGameState

-- Define the main function to run the game
main :: IO ()
main = play
    (InWindow "2048 Game" (800, 600) (100, 100)) -- Window settings
    white                                        -- Background color
    60                                           -- Frames per second
    initialState                                 -- Initial state
    render                                       -- Render function
    handleInput                                  -- Input handling function
    update                                       -- Update function

