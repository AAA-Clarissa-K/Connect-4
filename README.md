### **Connect 4 Project:**
Implementing the game Connect 4 where the user can play against a computer with the traditional rules of the game (on a 7x6 board).

We gave players the option to choose the opposing computer difficulty
- Hard (minimax algorithm)
- Medium (60% minimax algorithm, 40% randomized)
- Easy (randomized)

Our minimax was more defensive in the sense that it prioritizes taking the other player's most beneficial move (i.e completing the player's 4-in-a-row) over completing its own 4-in-a-row.

Something that can be improved is processing time of which move the computer should take, since Connect 4 has a large state search space than a game like Tic-Tac-Toe.
