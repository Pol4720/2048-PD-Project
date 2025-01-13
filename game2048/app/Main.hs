module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Lib (initialState, render, handleInput, update)

-- Definir la función principal para ejecutar el juego
main :: IO ()
main = play
    (InWindow "2048 Game" (800, 600) (100, 100)) -- Configuración de la ventana
    white                                        -- Color de fondo
    60                                           -- Fotogramas por segundo
    initialState                                 -- Estado inicial
    render                                       -- Función de renderizado
    handleInput                                  -- Función de manejo de entrada
    update                                       -- Función de actualización
