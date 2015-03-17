# Gomoku_AI
AI functions for Gomoku game.  Written in Scheme.


The AI is a position function that takes in the Gomoku map and returns an ideal to position to move to.  This AI strictly filters the best offensive moves and best defensive moves.  

###The tier works as such:

1.  Make Winning Moves

2.  Block Winning Moves

3.  Make four-in-row move

4.  Block four-in-row move

5.  Make three-in-row move

6.  Block three-in-row move

7.  Make two-in-row

8.  Random move
