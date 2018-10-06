
all : fens snp disps

fens : chess/board/main.cpp board.o
	g++ -o ~/bin/fens chess/board/main.cpp board.o -I/Users/btobrien/dev/chess_worm/cpp

snp : snap/main.cpp snap/snap_tree.h include/read.h
	g++ -o ~/bin/snp snap/main.cpp -I/Users/btobrien/dev/chess_worm/cpp

disps : shapes shapes2 pieces

shapes : chess/disp/main.cpp chess/disp/disp.h chess/disp/shapes.cpp chess/include/pieces.h
	g++ -o ~/bin/shapes chess/disp/main.cpp chess/disp/shapes.cpp -I/Users/btobrien/dev/chess_worm/cpp

shapes2 : chess/disp/main.cpp chess/disp/disp.h chess/disp/shapes2.cpp chess/include/pieces.h
	g++ -o ~/bin/shapes2 chess/disp/main.cpp chess/disp/shapes2.cpp -I/Users/btobrien/dev/chess_worm/cpp

pieces : chess/disp/main.cpp chess/disp/disp.h chess/disp/pieces.cpp chess/include/pieces.h
	g++ -o ~/bin/pieces chess/disp/main.cpp chess/disp/pieces.cpp -I/Users/btobrien/dev/chess_worm/cpp

border : chess/disp/main.cpp chess/disp/disp.h chess/disp/pieces.cpp chess/include/pieces.h
	g++ -o ~/bin/border chess/disp/border.cpp

board.o : chess/board/board.cpp chess/include/board.h chess/board/board_state.h chess/board/white.h chess/board/black.h chess/board/move.h chess/include/squares.h chess/include/pieces.h chess/include/fen.h chess/include/castle_flags.h
	g++ -c chess/board/board.cpp -I/Users/btobrien/dev/chess_worm/cpp

test :
	chess/board/test/run.sh; snap/test/run.sh

clean :
	rm board.o
