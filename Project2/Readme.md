# Anaash Game

## Group Information

### Group Designation
- **Group Name**: Anaash_9

### Group Members
1. **Lara Inês Alves Cunha**  
   - **Student Number**: [Insert Number]  
   - **Contribution**: 50%  
     - **Tasks Performed**:  
       - Implementation of the `initial_state/2` predicate for initializing the game state.
       - Development of the `display_game/1` predicate for visualizing the board and game state.
       - Creation of the `game_over/2` predicate to determine when the game ends and the winner.
       - Auxiliary predicates for game initialization and visualization.

2. **Bernardo Marques Soares da Costa**  
   - **Student Number**: [Insert Number]  
   - **Contribution**: 50%  
     - **Tasks Performed**:  
       - Implementation of the `move/3` predicate to validate and execute game moves.
       - Development of the `valid_moves/2` predicate to generate all valid moves for the current game state.
       - Creation of the `value/3` predicate to evaluate game states using heuristics.
       - Auxiliary predicates for move execution and game evaluation.

## Installation and Execution

### Prerequisites
Before running the game, ensure the following are installed:

1. **SICStus Prolog 4.9**  
   - Download and install from the [official website](https://sicstus.sics.se/).  
   - Follow the installation instructions for your operating system (Linux/Windows).

---

### Steps for Execution on Linux

1. **Install SICStus Prolog**:
   - Follow the official instructions for installing SICStus Prolog on Linux. Use the terminal to verify the installation:
     ```bash
     sicstus --version
     ```

2. **Download the Project**:
   - Download the game files to your desired directory:
     ```bash
     cd /path/to/anaash
     ```

3. **Start the Game**:
   - Open the terminal and navigate to the project directory.
   - Launch SICStus Prolog:
     ```bash
     sicstus
     ```
   - Load the game file:
     ```prolog
     ?- [game].
     ```
   - Start the game by typing:
     ```prolog
     ?- play.
     ```

---

### Steps for Execution on Windows

1. **Install SICStus Prolog**:
   - Download the SICStus Prolog installer for Windows from the [official website](https://sicstus.sics.se/).  
   - Run the installer and follow the setup wizard to complete the installation.

2. **Download the Project**:
   - Download the ZIP file of the project to your desired directory.

3. **Start the Game**:
   - Open SICStus Prolog via the Start Menu.
   - Use the `cd/1` predicate to navigate to the directory containing the game files:
     ```prolog
     ?- cd('C:/path/to/anaash').
     ```
   - Load the game file:
     ```prolog
     ?- [game].
     ```
   - Start the game by typing:
     ```prolog
     ?- play.
     ```

---

## Description of the Game

### Overview
**Anaash** is a two-player strategy game designed by **Mark Steere** in February 2021. The game is played on a square checkerboard (typically 6x6 or 8x8), where players control stacks of checkers of their respective colors, Red and Blue. The objective is to eliminate all the opponent's checkers by making positional moves, stacking friendly checkers, or capturing enemy stacks.

The name "Anaash" comes from the Mongolian word for "giraffe."

---

### Rules

#### Initial Setup
- The board is filled with a checkered pattern of Red and Blue stacks.
- Each stack starts with a height of **1** (called a singleton).
- Red begins the game, followed by Blue, and players alternate turns.

#### Types of Moves
Players can move one stack per turn. There are three types of moves:

1. **Positional Moves**:
   - Move a stack to an orthogonally adjacent, unoccupied square.
   - The destination must be one square closer (Manhattan distance) to the nearest stack, regardless of its color or height.
   - Only stacks with no orthogonal adjacencies (isolated stacks) can make positional moves.
   - Example: Refer to Figure 2 in the [rulebook](https://www.marksteeregames.com/Anaash_rules.pdf).

2. **Stacking Moves**:
   - Move a stack onto an orthogonally adjacent friendly stack of equal or larger height.
   - Example: Refer to Figure 3 in the [rulebook](https://www.marksteeregames.com/Anaash_rules.pdf).

3. **Capturing Moves**:
   - Capture an orthogonally adjacent enemy stack of equal or smaller height.
   - Example: Refer to Figure 4 in the [rulebook](https://www.marksteeregames.com/Anaash_rules.pdf).

#### Objective
- The game ends when one player captures all enemy checkers.
- There are no draws in Anaash, as at least one player will always have a valid move.

#### Additional Rules
- If a player has no available moves, they must pass their turn until a move becomes available.

---

## Considerations for Game Extensions

### Variable-Sized Boards
The game design has been implemented with support for variable-sized boards, allowing players to choose board dimensions between 5x5 and 10x10 during the initial game configuration. This flexibility was achieved by dynamically generating the board and adjusting the rules to accommodate different board sizes without impacting the gameplay mechanics. The `initial_state/2` predicate generates the board and ensures it adheres to the selected size, with all initial stacks properly positioned in the checkered pattern.

### Optional Rules
To enhance accessibility for players of different skill levels, the game design includes optional rules that can be toggled based on player preference. Examples include:
- **Simplified Rules for Novice Players**: These rules reduce the complexity of valid moves by limiting move options to basic positional moves. Capturing and stacking moves can be disabled to help new players familiarize themselves with the core mechanics.
- **Additional Rules for Expert Players**: Advanced players can enable strategic constraints, such as requiring specific conditions to be met before performing a stacking or capturing move. For example, expert rules could introduce restrictions based on stack heights or limit the number of consecutive captures.

These optional rules can be seamlessly integrated by extending the `valid_move/3` and `choose_move/3` predicates to account for the chosen difficulty level or rule set.

### AI Difficulty Levels
The game includes two AI difficulty levels:
1. **Level 1 (Random Moves)**: The AI selects valid moves randomly, providing a basic challenge for beginner players.
2. **Level 2 (Greedy Algorithm)**: The AI evaluates moves using the `value/3` predicate and chooses the move that maximizes its advantage, offering a more strategic challenge.

This modular design allows additional difficulty levels to be introduced in the future, such as implementing advanced AI algorithms (e.g., minimax or Monte Carlo Tree Search).

### Enhanced User Experience
Several design considerations were made to improve the overall user experience:
- **Interactive Menu**: The main menu provides intuitive navigation for game setup, including board size selection, game mode configuration, and difficulty level selection.
- **Dynamic Feedback**: Players receive real-time feedback on invalid moves, game states, and when turns are passed due to unavailable moves.
- **Visual Enhancements**: The board is displayed with labeled rows and columns, and stack information is clearly represented, making the game state easy to interpret.

### Future Extensions
The current design allows for the following potential extensions:
- **Online Multiplayer**: Integrating network play for human-vs-human matches over the internet.
- **Customizable Rules**: Adding a rule editor to enable players to create and test their own variations of the game.
- **Scenario-Based Gameplay**: Introducing predefined scenarios where players start with specific board configurations and objectives, such as capturing a certain number of opponent stacks within a limited number of moves.
- **Statistics and Leaderboards**: Tracking player performance, win rates, and AI difficulty success metrics to encourage competition and replayability.

These considerations ensure that the game design remains modular, scalable, and adaptable to future development needs.
