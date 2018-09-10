#include "AbstractDisplay.h"
#include "BoardDisplay.h"
using std::string;
using std::cin;

void main(int argc, char* argv) {

	bool is_flipped = (argc > 1 && argv[1] == "-f");

	string fen;
	while (getline(std::cin, fen)) {
		Board brd(fen);
		DisplayBoard(brd, is_flipped);
	};
}

