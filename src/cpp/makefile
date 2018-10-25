cflags = -std=c++11

all : fens snp disps splay border

fens : chess/board/main.cpp board.o
	g++ $(cflags) -o ~/bin/fens chess/board/main.cpp board.o -I`pwd`

snp : snap/main.cpp snap/snap_tree.h include/read.h
	g++ $(cflags) -o ~/bin/snp snap/main.cpp -I`pwd`

splay : splay/main.cpp splay/splay.cpp
	g++ $(cflags) -o ~/bin/splay splay/main.cpp splay/splay.cpp

disps : shapes shapes2 pieces

shapes : chess/disp/main.cpp chess/disp/disp.h chess/disp/shapes.cpp chess/include/pieces.h
	g++ $(cflags) -o ~/bin/shapes chess/disp/main.cpp chess/disp/shapes.cpp -I`pwd`

shapes2 : chess/disp/main.cpp chess/disp/disp.h chess/disp/shapes2.cpp chess/include/pieces.h
	g++ $(cflags) -o ~/bin/shapes2 chess/disp/main.cpp chess/disp/shapes2.cpp -I`pwd`

pieces : chess/disp/main.cpp chess/disp/disp.h chess/disp/pieces.cpp chess/include/pieces.h
	g++ $(cflags) -o ~/bin/pieces chess/disp/main.cpp chess/disp/pieces.cpp -I`pwd`

border : chess/disp/border.cpp
	g++ $(cflags) -o ~/bin/border chess/disp/border.cpp

board.o : chess/board/board.cpp chess/include/board.h chess/board/board_state.h chess/board/white.h chess/board/black.h chess/board/move.h chess/include/squares.h chess/include/pieces.h chess/include/fen.h chess/include/castle_flags.h
	g++ $(cflags) -c chess/board/board.cpp -I`pwd`

test :
	chess/board/test/run.sh; snap/test/run.sh

clean :
	rm board.o
