
all : brd snp disp

brd : chess/board/main.cpp board.o
	g++ -o ~/bin/brd chess/board/main.cpp board.o -I/Users/btobrien/dev/chess_worm/cpp

snp : snap/main.cpp snap/snap_tree.h include/read.h
	g++ -o ~/bin/snp snap/main.cpp -I/Users/btobrien/dev/chess_worm/cpp

disp : chess/disp/main.cpp chess/disp/brd_disp.h chess/disp/abstract.cpp chess/include/pieces.h
	g++ -o ~/bin/disp chess/disp/main.cpp chess/disp/abstract.cpp -I/Users/btobrien/dev/chess_worm/cpp

board.o : chess/board/board.cpp chess/include/board.h chess/board/board_state.h chess/board/white.h chess/board/black.h chess/board/move.h chess/include/squares.h chess/include/pieces.h chess/include/fen.h chess/include/castle_flags.h
	g++ -c chess/board/board.cpp -I/Users/btobrien/dev/chess_worm/cpp

clean :
	rm board.o
