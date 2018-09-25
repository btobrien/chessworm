
#include "brd_disp.h"
#include <string>

int main(int argc, char** argv) {

	bool is_flipped = (argc > 1 && std::string(argv[1]) == "-f");

	std::string fen;
	getline(std::cin, fen);
	DisplayFen(fen);

	return 0;
}

