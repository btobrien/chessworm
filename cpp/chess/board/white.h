
#pragma once

#include "board_state.h"
#include "chess/include/squares.h"
#include "chess/include/pieces.h"
#include "black.h"

class White  : public BoardState, public Chess::White {
public:

	static inline bool isOppPiece(char p) { return Chess::Black::isPiece(p); }
	static inline char oppPiece(char p) { return Chess::Black::piece(p); }

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
		if (!_flags.whiteCastleShort || _[f1] || _[g1]) 
			return false;
		if (isThreatened(e1) || isThreatened(f1) || isThreatened(g1)) 
			return false;
		_[f1] = White::ROOK;
		_[g1] = White::KING;
		_[h1] = Chess::nullpiece;
		_[e1] = Chess::nullpiece;
		_flags.whiteCastleShort = false;
		_flags.whiteCastleLong = false;
		return true;
	}

	bool TryCastleLong() {
		if (!_flags.whiteCastleLong || _[b1] || _[c1] || _[d1]) 
			return false;
		if (isThreatened(e1) || isThreatened(d1) || isThreatened(c1)) 
			return false;
		_[d1] = White::ROOK;
		_[c1] = White::KING;
		_[a1] = Chess::nullpiece;
		_[e1] = Chess::nullpiece;
		_flags.whiteCastleShort = false;
		_flags.whiteCastleLong = false;
		return true;
	}

	void SetEnPassant(int oldSquare, int newSquare) {
		_enPassant = nullsquare;
		if (_[newSquare] != PAWN || rank(oldSquare) != '2' || newSquare != (UP_UP + oldSquare))
			return;
		char oppPawn = oppPiece(Chess::PAWN);
		if (_[LEFT + newSquare] != oppPawn && _[RIGHT + newSquare] != oppPawn)
			return;
		_enPassant = UP + oldSquare;
	}

	void SetCastlingRights(int oldSquare, int newSquare) {
		switch (oldSquare) {
			case e1: 
				_flags.whiteCastleShort = false;
				_flags.whiteCastleLong = false;
				break;
			case a1: 
				_flags.whiteCastleLong = false;
				break;      
			case h1: 
				_flags.whiteCastleShort = false;
				break;      
		}
		switch (newSquare) {
			case a8: 
				_flags.blackCastleLong = false;
				break;      
			case h8: 
				_flags.blackCastleShort = false;
				break;      
		}
	}
};

