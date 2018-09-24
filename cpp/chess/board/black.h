
#pragma once

#include "board_state.h"
#include "chess/include/squares.h"
#include "chess/include/pieces.h"
#include "white.h"


class Black : public BoardState {
public:
	static const char PAWN = Chess::PAWN + SHIFT;
	static const char KNIGHT = Chess::KNIGHT + SHIFT;
	static const char BISHOP = Chess::BISHOP + SHIFT;
	static const char ROOK = Chess::ROOK + SHIFT;
	static const char QUEEN = Chess::QUEEN + SHIFT;
	static const char KING = Chess::KING + SHIFT;

	static inline bool isMyPiece(char p) {
		switch (p) { case PAWN: case KNIGHT: case BISHOP: case ROOK: case QUEEN: case KING: return true; }
		return false;
	}
	static inline bool isOppPiece(char p) { return p && !isMyPiece(p); }
	static inline char myPiece(char p) { return p + SHIFT; }
	static inline char oppPiece(char p) { return p; }
	static inline char whichPiece(char p) { return p - SHIFT; }

	static const int PAWN_DIRECTION = DOWN;

	bool isThreateningWithPawn(int square) const {
		int pawnSquare = UP_RIGHT + square;
		if (isSquare(pawnSquare) && _[pawnSquare] == Black::PAWN)
			return true;
		pawnSquare = UP_LEFT + square;
		if (isSquare(pawnSquare) && _[pawnSquare] == Black::PAWN)
			return true;
		return false;
	}

	bool isThreatened(int square) const {
		return isThreatening<White>(square);
	}

	bool TryCastleShort() {
		if (!_flags.blackCastleShort || _[f8] || _[g8]) 
			return false;
		if (isThreatening<White>(e8) || isThreatening<White>(f8) || isThreatening<White>(g8)) 
			return false;
		_[f8] = Black::ROOK;
		_[g8] = Black::KING;
		_[h8] = Chess::nullpiece;
		_[e8] = Chess::nullpiece;
		_flags.blackCastleShort = false;
		_flags.blackCastleLong = false;
		return true;
	}

	bool TryCastleLong() {
		if (!_flags.blackCastleLong || _[b8] || _[c8] || _[d8]) 
			return false;
		if (isThreatening<White>(e8) || isThreatening<White>(d8) || isThreatening<White>(c8)) 
			return false;
		_[d8] = Black::ROOK;
		_[c8] = Black::KING;
		_[a8] = Chess::nullpiece;
		_[e8] = Chess::nullpiece;
		_flags.blackCastleShort = false;
		_flags.blackCastleLong = false;
		return true;
	}

	void SetEnPassant(int oldSquare, int newSquare) {
		_enPassant = nullsquare;
		if (_[newSquare] != PAWN || rank(oldSquare) != '7' || newSquare != (DOWN_DOWN + oldSquare))
			return;
		char oppPawn = oppPiece(Chess::PAWN);
		if (_[LEFT + newSquare] != oppPawn && _[RIGHT + newSquare] != oppPawn)
			return;
		_enPassant = DOWN + oldSquare;
	}

	void SetCastlingRights(int oldSqaure, int newSquare) {
		switch (oldSqaure) {
			case e8: 
				_flags.blackCastleShort = false;
				_flags.blackCastleLong = false;
				break;      
			case a8: 
				_flags.blackCastleLong = false;
				break;      
			case h8: 
				_flags.blackCastleShort = false;
				break;      
		}
		switch (newSquare) {
			case a1: 
				_flags.whiteCastleLong = false;
				break;      
			case h1: 
				_flags.whiteCastleShort = false;
				break;      
		}
	}
};
