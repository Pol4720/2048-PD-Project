module GraphicsTests where

import Test.HUnit
import Graphics.Render (renderGrid)
import Game.Grid (emptyGrid)

-- Prueba para verificar que el renderizado de un tablero vacío no falla
testRenderEmptyGrid :: Test
testRenderEmptyGrid = TestCase $ do
    let grid = emptyGrid
    let picture = renderGrid grid
    assertBool "Render should not be empty" (picture /= Blank)

-- Ejecutar todas las pruebas
main :: IO Counts
main = runTestTT $ TestList [testRenderEmptyGrid]
