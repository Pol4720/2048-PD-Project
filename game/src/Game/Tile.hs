module Game.Tile where

-- Definimos el tipo Tile que representa una ficha en el juego 2048
data Tile = Tile { value :: Int } deriving (Eq, Show)

-- Función para crear una nueva ficha con un valor inicial
newTile :: Int -> Tile
newTile v = Tile { value = v }

-- Función para combinar dos fichas. Si tienen el mismo valor, se suman.
combineTiles :: Tile -> Tile -> Maybe Tile
combineTiles (Tile v1) (Tile v2)
    | v1 == v2  = Just (Tile (v1 + v2))
    | otherwise = Nothing

-- Función para verificar si una ficha está vacía (valor 0)
isEmpty :: Tile -> Bool
isEmpty (Tile v) = v == 0

-- Función para duplicar el valor de una ficha
doubleTile :: Tile -> Tile
doubleTile (Tile v) = Tile (v * 2)

-- Función para representar una ficha como una cadena de texto
tileToString :: Tile -> String
tileToString (Tile v) = show v