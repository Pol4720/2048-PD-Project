# 2048-PD-Project

## 2048 Game in Haskell

## Description

+ This repository contains a fully implemented version of the popular game 2048, developed as a final project for a declarative programming course. Built using Haskell, this project showcases functional programming principles and leverages Haskell's powerful type system.

## Project Structure

This folder contains the game's source code.(in 2048Master we have another 2048 game but without a visual interface)

+ **Game2048/**: Contains the main game logic.
  + **Main.hs**: Main controller that manages the game state and interactions between different components.
  Implements the game logic, such as moving and merging tiles, as well as checking if the player has won or lost.
  Also represents the game board, including its initialization and updates.
  Defines the structure of a tile in the game, including its value and position.
  Manages the game's scoring system.

  It also Handles the graphical part of the game.

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
   `cabal build`

Running the Game

To run the game, you can use:

`cabal run`

This will compile and run the Haskell application.

## How to Run the Game

Follow these steps:

1. Clone this repository to your local machine.
2. Navigate to the project folder.
3. Run `cabal setup` to configure GHC and dependencies.
4. Run `cabal run` to start the game.

Enjoy playing 2048 in Haskell!
