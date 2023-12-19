Devs: Yanjun Zhao, Jingtian Wu, Mark Chen, Kevin Zhen 

Description:  
  Our project is chess game with 3 players which is inspired by Legends of the 
  Three Kingdoms, a Chinese epic. Although It's similar to the classical 
  3-player chess by now, as our project develop, we will extend the chess board 
  and attached 3 square parts in the edge of the current board and more complex 
  playing method will be enabled. 

  Players will move their pieces by clicking on the pieces that they want to 
  move first, and then the position that they want to move to. They are allowed
  to make "illegal moves" though other players can force a redo by reporting them.
  Additionally, if one player captures the king of another player, they absorb 
  the loser's remaining pieces into their own force, giving them a power boost. 
  Finally. The game ends either when one player is remaining or if one of the 
  players manages to get their king to the opposite side of the board.

  Install instructions: 
  - make sure that git is installed, also make sure that ocaml and some ide/code editor that you use is capable of running this program
  - clone repo
  - do a "dune build" in command line before first use
  - input "make play" into terminal
  - a new pop-up window should appear with the chessboard!
  - ENJOY GAMING

