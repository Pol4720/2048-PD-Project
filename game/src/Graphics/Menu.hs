module Graphics.Menu (
    displayMenu,
    handleMenuChoice,
    changeBoardSize,
    changeWinningTile,
    changeStartingTiles
) where

-- Function to display the menu
displayMenu :: IO ()
displayMenu = do
    putStrLn "2048 Game Configuration Menu"
    putStrLn "1. Change Board Size"
    putStrLn "2. Change Winning Tile"
    putStrLn "3. Change Starting Tiles"
    putStrLn "4. Exit"
    putStrLn "Enter your choice: "
    choice <- getLine
    handleMenuChoice choice

-- Function to handle menu choices
handleMenuChoice :: String -> IO ()
handleMenuChoice choice = case choice of
    "1" -> changeBoardSize
    "2" -> changeWinningTile
    "3" -> changeStartingTiles
    "4" -> putStrLn "Exiting menu..." >> return ()
    _   -> do
        putStrLn "Invalid choice, please try again."
        displayMenu

-- Function to change the board size
changeBoardSize :: IO ()
changeBoardSize = do
    putStrLn "Enter new board size (e.g., 4 for 4x4): "
    size <- getLine
    putStrLn $ "Board size changed to " ++ size ++ "x" ++ size
    displayMenu

-- Function to change the winning tile
changeWinningTile :: IO ()
changeWinningTile = do
    putStrLn "Enter new winning tile (e.g., 2048): "
    tile <- getLine
    putStrLn $ "Winning tile changed to " ++ tile
    displayMenu

-- Function to change the starting tiles
changeStartingTiles :: IO ()
changeStartingTiles = do
    putStrLn "Enter number of starting tiles: "
    tiles <- getLine
    putStrLn $ "Number of starting tiles changed to " ++ tiles
    displayMenu
