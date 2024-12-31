# 2048-PD-Project

This project is a Haskell implementation of the 2048 game.

## Building and Running

To build and run the project, use the following commands:

```sh
stack setup
stack build
stack exec 2048-PD-Project
```

If you encounter missing module errors, you may need to install the following dependencies:

```sh
stack install random
stack install gloss
```

## Project Structure

The project is organized as follows:

- `app/`: Contains the `Main.hs` file, which is the entry point of the application.
- `src/`: Contains the source code for the game.
- `test/`: Contains the test suite for the project.
- `game/`: Contains additional resources and configuration files.

## Dependencies

The project dependencies are specified in the `stack.yaml` and `package.yaml` files.
