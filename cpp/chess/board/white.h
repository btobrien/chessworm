
#pragma once

#include "board_state.h"
#include "chess/include/squares.h"
#include "chess/include/pieces.h"
#include "black.h"

class White  : public BoardState {
public:
	static const int SHIFT = ('a' - 'A');
	static const char PAWN = Chess::PAWN;
	static const char KNIGHT = Chess::KNIGHT;
	static const char BISHOP = Chess::BISHOP;
	static const char ROOK = Chess::ROOK;
	static const char QUEEN = Chess::QUEEN;
	static const char KING = Chess::KING;

	static inline bool isMyPiece(char p) {
		switch (p) { case PAWN: case KNIGHT: case BISHOP: case ROOK: case QUEEN: case KING: return true; }
		return false;
	}
	static inline bool isOppPiece(char p) { return p && !isMyPiece(p); }
	static inline char myPiece(char p) { return p; }
	static inline char oppPiece(char p) { return p + SHIFT; }
	static inline char whichPiece(char p) { return p; }

	static const int PAWN_DIRECTION = UP;

	bool isThreateningWithPawn(int square) const {
		int pawnSquare = DOWN_RIGHT + square;
		if (isSquare(pawnSquare) && _[pawnSquare] == PAWN)
			return true;
		pawnSquare = DOWN_LEFT + square;
		if (isSquare(pawnSquare) && _[pawnSquare] == PAWN)
			return true;
		return false;
	}

	bool isThreatened(int square) const {
		return isThreatening<Black>(square);
	}

	bool TryCastleShort() {
		if (!_whiteCastleShort || _[f1] || _[g1]) 
			return false;
		if (isThreatening<Black>(e1) || isThreatening<Black>(f1) || isThreatening<Black>(g1)) 
			return false;
		_[f1] = White::ROOK;
		_[g1] = White::KING;
		_[h1] = Chess::nullpiece;
		_[e1] = Chess::nullpiece;
		_whiteCastleShort = false;
		_whiteCastleLong = false;
		return true;
	}

	bool TryCastleLong() {
		if (!_whiteCastleLong || _[b1] || _[c1] || _[d1]) 
			return false;
		if (isThreatening<Black>(e1) || isThreatening<Black>(d1) || isThreatening<Black>(c1)) 
			return false;
		_[d1] = White::ROOK;
		_[c1] = White::KING;
		_[a1] = Chess::nullpiece;
		_[e1] = Chess::nullpiece;
		_whiteCastleShort = false;
		_whiteCastleLong = false;
		return true;
	}

	void SetEnPassant(int fromSquare, int toSquare) {
		if (_[toSquare] == PAWN && rank(fromSquare) == '2' && toSquare == (UP_UP + fromSquare)) {
			_enPassant = UP + fromSquare;
			return;
		}
		_enPassant = nullsquare;
	}

	void SetCastlingRights(int fromSquare, int toSquare) {
		switch (fromSquare) {
			case e1: 
				_whiteCastleShort = false;
				_whiteCastleLong = false;
				break;
			case a1: 
				_whiteCastleLong = false;
				break;      
			case h1: 
				_whiteCastleShort = false;
				break;      
		}
		switch (toSquare) {
			case a8: 
				_blackCastleLong = false;
				break;      
			case h8: 
				_blackCastleShort = false;
				break;      
		}
	}
};

