module Graphics.Graphics (
    -- Lista de exportación
    window,
    backgroundColor,
    fps
) where

import Graphics.Gloss

-- Configuración general para la biblioteca Gloss
window :: Display
window = InWindow "2048 Game" (800, 600) (100, 100)

backgroundColor :: Color
backgroundColor = white

fps :: Int
fps = 60
