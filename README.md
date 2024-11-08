# Ploy Bot - Haskell Implementation

This project is a Haskell-based bot for the strategy board game **Ploy**. The bot can make strategic moves within the game and includes unit testing to ensure code reliability. The assignment originates from a university coursework but has been modified here to suit a project-oriented approach.

## Overview

**Ploy** is a board game similar to chess, designed for two players. Each player has pieces with different movement capabilities. The objective is to capture the opponent’s Commander or all their pieces. This project provides a Haskell implementation of a bot that can play **Ploy**, including essential functionality to interpret and validate game moves.

## Game Rules (Simplified)

- **Board**: The game is played on a 9x9 grid.
- **Pieces**: Each player has 15 pieces initially, with different movement abilities:
  - **Shield**: Moves 1 field or rotates.
  - **Probe**: Moves up to 2 fields or rotates.
  - **Lance**: Moves up to 3 fields or rotates.
  - **Commander**: Moves 1 field or rotates.
- **Objective**: The game ends when a player captures the opponent's Commander or removes all other pieces, with the last move determining the winner.

## Project Structure

This project is organized into two primary modules:

1. **Board**: Contains functions to validate, parse, and manage the game board.
2. **Ploy**: Contains game logic, including functions for move validation and endgame detection.

### Key Functions

1. **validateFEN**: Validates the initial board state string based on a custom FEN (Forsyth–Edwards Notation) format.
2. **buildBoard**: Builds the board state from a FEN string.
3. **line**: Determines the path between two positions on the board.
4. **gameFinished**: Checks if the game is over.
5. **isValidMove**: Validates if a move is legal based on the current game state.
6. **possibleMoves**: Lists possible moves for a piece on the board.
7. **listMoves**: Lists all possible moves for a player.

## Project Requirements

- Haskell Stack setup
- Modules **Board** and **Ploy** located in `src` directory
- Unit tests in `test` directory using `Test.HSpec`

## Getting Started

1. **Clone the repository**
   ```bash
   git clone https://github.com/your-username/ploy-bot.git
   cd ploy-bot
2. **Build the project**
   ```bash
   stack build
   
3. **Run Unit Tests**
   ```bash
   stack test ploy:units

4. **Execute the Bot**
   ```bash
   stack exec ploy

