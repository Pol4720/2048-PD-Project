# 2048-PD-Project

## 2048 Game in Haskell

## Description

+ This repository contains a fully implemented version of the popular game 2048, developed as a final project for a declarative programming course. Built using Haskell, this project showcases functional programming principles and leverages Haskell's powerful type system.

## Project Structure

### game/

+ **stack.yaml**: Configuration file for Stack, including the dependencies needed to compile and run the game.

#### assets/

This folder stores all the resources used in the game.

+ **images/**: Images used in the game (e.g., tile sprites).
+ **sounds/**: Sound files for effects or background music.
+ **fonts/**: Fonts used to display text in the game.

#### app/

+ **Main.hs**: This is the entry point of the program. Here the game is initialized, event loops are set up, and execution begins.

#### src/

This folder contains the game's source code.

+ **Game/**: Contains the main game logic.
  + **Game.hs**: Main controller that manages the game state and interactions between different components.
  + **GameLogic.hs**: Implements the game logic, such as moving and merging tiles, as well as checking if the player has won or lost.
  + **Grid.hs**: Represents the game board, including its initialization and updates.
  + **Tile.hs**: Defines the structure of a tile in the game, including its value and position.
  + **Score.hs**: Manages the game's scoring system.

+ **Graphics/**: Handles the graphical part of the game.
  + **Graphics.hs**: General configurations related to the Gloss library.
  + **Render.hs**: Functions to render the board, tiles, and other visual elements on the screen.
  + **InputHandler.hs**: Manages user input events, such as key presses.

+ **Config/**: Contains general game configurations.
  + **Config.hs**: Defines constants and configurations such as board size, colors, etc.

#### tests/

This folder contains unit tests and integration tests to ensure code quality.

+ **GameTests.hs**: Tests related to the game logic.
+ **GraphicsTests.hs**: Tests related to the graphical part.

#### Other Files

+ **README.md**: This file provides information about the project, its structure, and how to run it.

### Development and Execution

To develop and run the game using Visual Studio Code (VSCode), you need some specific extensions and tools. Below are the requirements and installation methods from the terminal.

#### Requirements

1. Haskell Platform: You need to have Haskell installed on your system. This includes the GHC compiler and the Cabal package manager.

2. VSCode Extensions:

   • Haskell Language Server: Provides support for Haskell editing, including autocompletion, type checking, and more.

   • Haskell Syntax Highlighting: Enhances the readability of Haskell code with syntax highlighting.

   • Code Runner (optional): Allows you to run code snippets directly from the editor.

#### Installation of Requirements

1. Installing Haskell

Depending on your operating system, you can install Haskell using GHCup, which is a recommended tool for managing Haskell installations. Here is how to do it:

On macOS or Linux

Open the terminal and run:

`curl --proto '=https' --tlsv1.2 -sSf <https://get.haskellstack.org/> | sh`

This will install Stack, which is a project manager for Haskell and will also install GHC and Cabal if they are not present.

On Windows

You can use the GHCup installer available on its official page (<https://www.haskell.org/downloads/>). Download the installer and follow the instructions.

2. Installing VSCode Extensions

Once you have VSCode installed, you can install the necessary extensions from the terminal or directly from the graphical interface.

From the terminal

If you have the VSCode command line (code) set up, you can use the following commands:

`code --install-extension haskell.haskell`

`code --install-extension formulahendry.code-runner`

From the Graphical Interface

1. Open VSCode.

2. Go to the extensions section (you can use the shortcut Ctrl + Shift + X).

3. Search for "Haskell" and select "Haskell Language Server" to install it.

4. Search for "Code Runner" and install it if you want.

Project Configuration

Once you have everything installed:

1. Open the project and navigate to the game folder in VSCode:

   `code .`

2. Install the necessary dependencies by running:

   `stack setup`

   `stack build`

Running the Game

To run the game, you can use:

`stack run`

This will compile and run the Haskell application.

## How to Run the Game

Follow these steps:

1. Clone this repository to your local machine.
2. Navigate to the project folder.
3. Run `stack setup` to configure GHC and dependencies.
4. Run `stack run` to start the game.

Enjoy playing 2048 in Haskell!
