
#include "chess/include/pieces.h"
#include <iostream>

using namespace Chess;

void DisplaySquare(char p, bool isLightSquare) {
	std::cout << ' ';
	switch(p) {
		case White::PAWN:		    std::cout << "\u25B2"; break;
		case White::KNIGHT:		    std::cout << "\u25E4"; break;
		case White::BISHOP:		    std::cout << "\u25C6"; break;
		case White::ROOK:		    std::cout << "\u25A0"; break;
		case White::QUEEN:		    std::cout << "\u25CF"; break;
		case White::KING:		    std::cout << "\u25C9"; break;
		case Black::PAWN:		    std::cout << "\u25B3"; break;
		case Black::KNIGHT:		    std::cout << "\u25F8"; break;
		case Black::BISHOP:		    std::cout << "\u25C7"; break;
		case Black::ROOK:		    std::cout << "\u25A1"; break;
		case Black::QUEEN:		    std::cout << "\u25CB"; break;
		case Black::KING:		    std::cout << "\u25CE"; break;
		case ' ':                   std::cout << "\u00B7"; break;
		default:					std::cout << "X"; break;
	}
	std::cout << ' ';
}
