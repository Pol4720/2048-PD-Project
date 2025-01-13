module Config.Config (
    -- Lista de exportación
    boardSize,
    tileSize,
    backgroundColor
) where

-- Define constantes y configuraciones generales del juego
boardSize :: Int
boardSize = 4

tileSize :: Int
tileSize = 100

backgroundColor :: (Float, Float, Float)
backgroundColor = (1, 1, 1) -- Blanco
