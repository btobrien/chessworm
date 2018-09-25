
#include "chess/include/board.h"
#include <iostream>
#include <string>
#include <sstream>

using std::string;
using std::stringstream;
using std::cin;
using std::cout;
using std::cerr;
using std::endl;

int main(int argc, char* argv[]) {
	string init = (argc > 1) ? string(argv[1]) : "";
	Board brd(init);
	string move;
	while (getline(cin, move)) {
		stringstream ss(move);
		while(getline(ss, move, '/')) {
			if (move.empty())
				continue;
			if (brd.TryMove(move))
				cout << fen::to_string(brd) << endl;
			else {
				cerr << "ERROR: move=" << move << " failed" << endl;
				return 1;
			}
		}
	}
}
