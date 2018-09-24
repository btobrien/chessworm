
#pragma once

#include "board_state.h"
#include "chess/include/squares.h"
#include "chess/include/pieces.h"
#include "white.h"


class Black : public BoardState, public Chess::Black {
public:

	static inline bool isOppPiece(char p) { return Chess::White::isPiece(p); }
	static inline char oppPiece(char p) { return Chess::White::piece(p); }

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
		if (isThreatened(e8) || isThreatened(f8) || isThreatened(g8)) 
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
		if (isThreatened(e8) || isThreatened(d8) || isThreatened(c8)) 
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
