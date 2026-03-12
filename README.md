# 2048 — Haskell Implementation

<div align="center">

**The Classic 2048 Puzzle Game, Built in Pure Haskell**

A functional programming implementation of the 2048 sliding tile puzzle, featuring immutable state management, algebraic data types, and a terminal-based interface.

[![Haskell](https://img.shields.io/badge/Haskell-5D4F85?style=flat-square&logo=haskell&logoColor=white)](https://www.haskell.org)

</div>

---

## Overview

A faithful implementation of Gabriele Cirulli's [2048 game](https://play2048.co/) written entirely in **Haskell**, embracing the functional programming paradigm. The game runs in the terminal and demonstrates core Haskell concepts: pure functions, monadic I/O, pattern matching, list comprehensions, and algebraic data types.

## Gameplay

Slide numbered tiles on a 4×4 grid. When two tiles with the same number collide, they merge into one with their sum. The goal is to create a tile with the value **2048**.

```
┌──────┬──────┬──────┬──────┐
│      │      │      │      │
│      │   2  │      │      │
│      │      │      │      │
├──────┼──────┼──────┼──────┤
│      │      │      │      │
│      │      │   4  │      │
│      │      │      │      │
├──────┼──────┼──────┼──────┤
│      │      │      │      │
│   2  │      │      │   8  │
│      │      │      │      │
├──────┼──────┼──────┼──────┤
│      │      │      │      │
│      │      │   2  │   4  │
│      │      │      │      │
└──────┴──────┴──────┴──────┘
Score: 84
```

**Controls**: `w` (up) · `a` (left) · `s` (down) · `d` (right) · `q` (quit)

## Functional Design

### Core Concepts Used
- **Immutable Board State**: The 4×4 grid as a list of lists, never mutated
- **Pure Functions**: Slide, merge, and transpose operations with no side effects
- **Monadic I/O**: `IO` monad for user input and terminal rendering
- **Pattern Matching**: Exhaustive matching for move directions and tile operations
- **Random Number Generation**: `System.Random` for spawning new tiles (2 or 4)
- **Higher-Order Functions**: Extensive use of `map`, `filter`, `foldl`, `zipWith`

### Architecture

```
Main.hs          Entry point and game loop (IO monad)
Game.hs          Core game logic (pure functions)
Board.hs         Board representation and operations
Render.hs        Terminal rendering and formatting
```

## Building & Running

### Prerequisites

- GHC (Glasgow Haskell Compiler) 8.10+
- Cabal or Stack

### With Cabal

```bash
git clone https://github.com/Pol4720/2048-PD-Project.git
cd 2048-PD-Project

cabal build
cabal run
```

### With Stack

```bash
stack build
stack run
```

## Academic Context

Developed as a **Declarative Programming** course project at the University of Havana, Faculty of Mathematics and Computer Science (MATCOM).

## License

This project is licensed under the MIT License.
