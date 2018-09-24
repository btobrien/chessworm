
all : brd snp

brd : chess/board/main.cpp board.o move.o squares.o pieces.o castle_flags.o
	g++ -o ~/bin/brd chess/board/main.cpp board.o move.o squares.o pieces.o castle_flags.o -I/Users/btobrien/dev/chess_worm/cpp

snp : snap/main.cpp snap/snap_tree.h include/read.h
	g++ -o ~/bin/snp snap/main.cpp -I/Users/btobrien/dev/chess_worm/cpp

board.o : chess/board/board.cpp chess/include/board.h chess/board/board_state.h chess/board/move.h chess/include/squares.h chess/include/pieces.h chess/include/fen.h
	g++ -c chess/board/board.cpp -I/Users/btobrien/dev/chess_worm/cpp

move.o : chess/board/move.cpp chess/board/move.h chess/include/squares.h chess/include/pieces.h
	g++ -c chess/board/move.cpp -I/Users/btobrien/dev/chess_worm/cpp

squares.o : chess/include/squares.cpp chess/include/squares.h
	g++ -c chess/include/squares.cpp -I/Users/btobrien/dev/chess_worm/cpp

pieces.o : chess/include/pieces.cpp chess/include/pieces.h
	g++ -c chess/include/pieces.cpp -I/Users/btobrien/dev/chess_worm/cpp

castle_flags.o : chess/include/castle_flags.cpp chess/include/castle_flags.h
	g++ -c chess/include/castle_flags.cpp -I/Users/btobrien/dev/chess_worm/cpp

clean :
	rm *.o
