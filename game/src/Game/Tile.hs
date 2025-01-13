{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.Tile (
    -- Lista de exportación
    Tile(..),
    initialTile,
    mergeTiles,
    isEmpty,
    doubleTile,
    tileToString
) where

-- Definición del tipo de dato Tile
data Tile = Empty | Tile { value :: Int } deriving (Eq, Show)

-- Función para crear una nueva ficha con un valor inicial
initialTile :: Int -> Tile
initialTile v = Tile { value = v }

-- Función para combinar dos fichas. Si tienen el mismo valor, se suman.
mergeTiles :: Tile -> Tile -> Maybe Tile
mergeTiles (Tile v1) (Tile v2)
    | v1 == v2  = Just (Tile (v1 + v2))
    | otherwise = Nothing
mergeTiles _ _ = Nothing

-- Función para verificar si una ficha está vacía
isEmpty :: Tile -> Bool
isEmpty Empty = True
isEmpty (Tile v) = v == 0

-- Función para duplicar el valor de una ficha
doubleTile :: Tile -> Tile
doubleTile Empty = Empty
doubleTile (Tile v) = Tile (v * 2)

-- Función para representar una ficha como una cadena de texto
tileToString :: Tile -> String
tileToString Empty = "Empty"
tileToString (Tile v) = show v