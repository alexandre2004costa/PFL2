# PFL - Practical Assignment 2

![Prolog](https://img.shields.io/badge/Language-Prolog-blue)
![Status](https://img.shields.io/badge/Status-Completed-green)

## 📌 Project Description

This repository contains the implementation of **Blinq**, a two-player board game, developed in **Prolog** as part of the **Functional and Logic Programming (PFL)** course at **FEUP**.

### 🔹 Game Overview:
- **Objective**: Connect two opposite sides of the board with a continuous chain of your assigned color.
- **Players**: Player 1 (White) connects **Top to Bottom**, Player 2 (Black) connects **Left to Right**.
- **Board**: Square grid with stackable blocks, each divided into two colors.
- **Moves**: Players choose a piece, place it on the board, and attempt to form a connected path.
- **Winning Condition**: First player to connect their two sides wins. A draw occurs if no valid moves remain.

---

## 🛠️ Installation & Execution

### **🔹 Requirements**
- Install **SICStus Prolog 4.9** from [SICStus Download](https://sicstus.sics.se/download4.html).

### **🔹 Running the Game**
1. Download and extract the project folder.
2. Open a terminal and navigate to the `src` directory:
   ```sh
   cd PFL_TP2_T02_Blinq_04/src
   ```
3. Start **SICStus Prolog**:
   ```sh
   sicstus
   ```
4. Load the game file:
   ```prolog
   ?- ['game.pl'].
   ```
5. Start the game:
   ```prolog
   ?- play.
   ```

---

## 🎮 Game Logic

### **🔹 Game State Representation**
The game state consists of:
- **Board Grid**: A 2D list representing cell states (`'S'` for empty, `'W'` for white, `'B'` for black).
- **Levels**: Another 2D list tracking the stack height of each cell.
- **Moves Left**: The number of remaining moves.
- **Players**: Current player, opponent, and assigned colors.

### **🔹 Move Representation**
- Moves are represented as `[N, X, Y]` where:
  - `N` is the piece type (1-4).
  - `(X, Y)` is the target position.
- Pieces must align with a 2x2 grid when stacked.
- The board updates by applying the **`update_piece`** function four times (for 2x2 blocks).

### **🔹 User Interaction**
- **Main Menu**:
  1. Play (Human vs Human, Human vs AI, AI vs AI)
  2. Game Configurations (Board size, colors, style)
  3. Exit
- **Move Validation**: Ensures all moves are within board boundaries and follow stacking rules.

---

## 🔮 Extensions
- Supports dynamic board sizes (4x4, 5x5).
- Customizable colors and different boards.
- 2 levels of AI:
    - level 1 : plays random valid move.
    - level 2 : plays the best move taking into consideration an evaluation of the game state
---

## 📷 Screenshots
![Imagem 1](https://github.com/user-attachments/assets/8d9a3379-8136-42be-b3ea-274969982dfa)
![Imagem 2](https://github.com/user-attachments/assets/4a16c7ed-ec0c-44bd-b48e-e2d1494c1f23)

---

## 👥 Contributors

- **Alexandre Torres Costa** (50%) – Game over logic, move validation, winning conditions, color mechanics.
- **Sofia Afonso Gonçalves** (50%) – UI design, board rendering, input validation, value mechanics.

---

## 📜 References

- **Game Rules**: [NestorGames](https://nestorgames.com/rulebooks/BLINQ_A5_EN.pdf), [BoardGameGeek](https://boardgamegeek.com/boardgame/271266/blinq)
- **Prolog Documentation**: [SICStus Library](https://sicstus.sics.se/sicstus/docs/latest4/html/sicstus.html)

---

🛠️ Developed for the **Functional and Logic Programming (PFL)** course at **FEUP**.
