
#include "pieces.h"

bool Chess::isPiece(char p) {
	switch (p) { case PAWN: case KNIGHT: case BISHOP: case ROOK: case QUEEN: case KING: return true; }
	return false;
}

bool Chess::isCastleLong(std::string move) {
	if (move.length() < 5)
		return false;
	move = move.substr(0,5);
	return (move == "o-o-o" || move == "O-O-O");
}

bool Chess::isCastleShort(std::string move) {
	if (move.length() < 3)
		return false;
	move = move.substr(0,3);
	return (move == "o-o" || move == "O-O");
}

bool Chess::White::isPiece(char p) {
	switch (p) { case PAWN: case KNIGHT: case BISHOP: case ROOK: case QUEEN: case KING: return true; }
	return false;
}
char Chess::White::piece(char p) { return p; }
char Chess::White::whichPiece(char p) { return p; }

bool Chess::Black::isPiece(char p) {
	switch (p) { case PAWN: case KNIGHT: case BISHOP: case ROOK: case QUEEN: case KING: return true; }
	return false;
}
char Chess::Black::piece(char p) { return p + SHIFT; }
char Chess::Black::whichPiece(char p) { return p - SHIFT; }
