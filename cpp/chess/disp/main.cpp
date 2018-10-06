
#include "brd_disp.h"
#include <string>

int main(int argc, char** argv) {
	std::string fen;
	getline(std::cin, fen);
	if (fen.empty() || fen[0] == '-')
		fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -";
	DisplayFen(fen);
	return 0;
}

