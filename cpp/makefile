
all : fens snp disps

fens : chess/board/main.cpp board.o
	g++ -o ~/bin/fens chess/board/main.cpp board.o -I/Users/btobrien/dev/chess_worm/cpp

snp : snap/main.cpp snap/snap_tree.h include/read.h
	g++ -o ~/bin/snp snap/main.cpp -I/Users/btobrien/dev/chess_worm/cpp

disps : abstract abstract2 pieces

abstract : chess/disp/main.cpp chess/disp/brd_disp.h chess/disp/abstract.cpp chess/include/pieces.h
	g++ -o ~/bin/abstract chess/disp/main.cpp chess/disp/abstract.cpp -I/Users/btobrien/dev/chess_worm/cpp

abstract2 : chess/disp/main.cpp chess/disp/brd_disp.h chess/disp/abstract.cpp chess/include/pieces.h
	g++ -o ~/bin/abstract2 chess/disp/main.cpp chess/disp/abstract2.cpp -I/Users/btobrien/dev/chess_worm/cpp

pieces : chess/disp/main.cpp chess/disp/brd_disp.h chess/disp/pieces.cpp chess/include/pieces.h
	g++ -o ~/bin/pieces chess/disp/main.cpp chess/disp/pieces.cpp -I/Users/btobrien/dev/chess_worm/cpp

border : chess/disp/main.cpp chess/disp/brd_disp.h chess/disp/pieces.cpp chess/include/pieces.h
	g++ -o ~/bin/border chess/disp/border.cpp

board.o : chess/board/board.cpp chess/include/board.h chess/board/board_state.h chess/board/white.h chess/board/black.h chess/board/move.h chess/include/squares.h chess/include/pieces.h chess/include/fen.h chess/include/castle_flags.h
	g++ -c chess/board/board.cpp -I/Users/btobrien/dev/chess_worm/cpp

test :
	chess/board/test/run.sh; snap/test/run.sh

clean :
	rm board.o
