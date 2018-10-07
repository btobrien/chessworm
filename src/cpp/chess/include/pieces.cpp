
#include "pieces.h"

inline bool Chess::isPiece(char p) {
	switch (p) { case PAWN: case KNIGHT: case BISHOP: case ROOK: case QUEEN: case KING: return true; }
	return false;
}

inline bool Chess::isCastleLong(std::string move) {
	if (move.length() < 5)
		return false;
	move = move.substr(0,5);
	return (move == "o-o-o" || move == "O-O-O");
}

inline bool Chess::isCastleShort(std::string move) {
	if (move.length() < 3)
		return false;
	move = move.substr(0,3);
	return (move == "o-o" || move == "O-O");
}

inline bool Chess::White::isPiece(char p) {
	switch (p) { case PAWN: case KNIGHT: case BISHOP: case ROOK: case QUEEN: case KING: return true; }
	return false;
}
inline char Chess::White::piece(char p) { return p; }
inline char Chess::White::whichPiece(char p) { return p; }

inline bool Chess::Black::isPiece(char p) {
	switch (p) { case PAWN: case KNIGHT: case BISHOP: case ROOK: case QUEEN: case KING: return true; }
	return false;
}
inline char Chess::Black::piece(char p) { return p + SHIFT; }
inline char Chess::Black::whichPiece(char p) { return p - SHIFT; }
