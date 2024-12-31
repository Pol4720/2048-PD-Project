module GameTests where

import Test.HUnit
import Game.GameLogic (moveTiles, mergeTiles)
import Game.Grid (Grid, emptyGrid, updateGrid)
import Game.Tile (Tile(..))

-- Prueba para verificar el movimiento de tiles
testMoveTiles :: Test
testMoveTiles = TestCase $ do
    let grid = updateGrid emptyGrid 0 0 (Tile 2)
    let newGrid = moveTiles grid
    assertBool "Tile should move" (newGrid /= grid)

-- Prueba para verificar la fusión de tiles
testMergeTiles :: Test
testMergeTiles = TestCase $ do
    let grid = updateGrid (updateGrid emptyGrid 0 0 (Tile 2)) 0 1 (Tile 2)
    let newGrid = mergeTiles grid
    assertBool "Tiles should merge" (newGrid /= grid)

-- Ejecutar todas las pruebas
main :: IO Counts
main = runTestTT $ TestList [testMoveTiles, testMergeTiles]
