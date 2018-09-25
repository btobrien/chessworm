#pragma once

#include <string>

namespace Chess {

const char PAWN = 'P';
const char KNIGHT = 'N';
const char BISHOP = 'B';
const char ROOK = 'R';
const char QUEEN = 'Q';
const char KING = 'K';
const char nullpiece = 0;  // MUST BE 0 for nullness

bool isPiece(char p);
bool isCastleLong(std::string move);
bool isCastleShort(std::string move);

class White {
public:
	static const char PAWN = Chess::PAWN;
	static const char KNIGHT = Chess::KNIGHT;
	static const char BISHOP = Chess::BISHOP;
	static const char ROOK = Chess::ROOK;
	static const char QUEEN = Chess::QUEEN;
	static const char KING = Chess::KING;

	static bool isPiece(char);
	static char piece(char);
	static char whichPiece(char);
};

class Black {
private:
	static const int SHIFT = ('a' - 'A');
public:
	static const char PAWN = Chess::PAWN + SHIFT;
	static const char KNIGHT = Chess::KNIGHT + SHIFT;
	static const char BISHOP = Chess::BISHOP + SHIFT;
	static const char ROOK = Chess::ROOK + SHIFT;
	static const char QUEEN = Chess::QUEEN + SHIFT;
	static const char KING = Chess::KING + SHIFT;

	static bool isPiece(char);
	static char piece(char);
	static char whichPiece(char);
};

}

#include "pieces.cpp"
