#include "chess/include/pieces.h"
#include <iostream>
using namespace Chess;

void DisplayPiece(char piece) {
	switch(piece) {
		case White::PAWN:		    std::cout << "\u265F"; break;
		case White::KNIGHT:		    std::cout << "\u265E"; break;
		case White::BISHOP:		    std::cout << "\u265D"; break;
		case White::ROOK:		    std::cout << "\u265C"; break;
		case White::QUEEN:		    std::cout << "\u265B"; break;
		case White::KING:		    std::cout << "\u265A"; break;
		case Black::PAWN:		    std::cout << "\u2659"; break;
		case Black::KNIGHT:		    std::cout << "\u2658"; break;
		case Black::BISHOP:		    std::cout << "\u2657"; break;
		case Black::ROOK:		    std::cout << "\u2656"; break;
		case Black::QUEEN:		    std::cout << "\u2655"; break;
		case Black::KING:		    std::cout << "\u2654"; break;
		case ' ':                   std::cout << "\u00B7"; break;
		default:					std::cout << 'X';      break;
	}
}
