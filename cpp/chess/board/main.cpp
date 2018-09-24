
#include "chess/include/board.h"
#include "chess/include/fen.h"
#include "include/read.h"
#include <iostream>
#include <string>

using std::string;
using std::cin;
using std::cout;
using std::cerr;
using std::endl;

int main(int argc, char* argv[]) {
	Board brd;
	string move;
	while (getline(cin, move)) {
		move = getword(move);
		if (move.empty())
			continue;
		if (brd.TryMove(move))
			cout << fen::to_string(brd, brd.flags(), brd.clock(), brd.enPassant()) << endl;
		else {
			cerr << "ERROR: move=" << move << " failed" << endl;
			return 1;
		}
	}
}
