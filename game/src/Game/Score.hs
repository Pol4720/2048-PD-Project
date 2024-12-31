module Game.Score where

-- Maneja el sistema de puntuación del juego
type Score = Int

-- Inicializa la puntuación
initialScore :: Score
initialScore = 0

-- Actualiza la puntuación al fusionar tiles
updateScore :: Score -> Int -> Score
updateScore score value = score + value
