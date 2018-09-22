

#pragma once

#include "Chess.h"
#include "BoardState.h"

class Black : public BoardState {
public:
	bool isThreateningWithPawn(int square) {
		auto pawnSquare = UP_RIGHT + square;
		if (IsSquare(pawnSquare) && [pawnSquare] == Black::PAWN)
			return true;
		pawnSquare = UP_LEFT + square;
		if (IsSquare(pawnSquare) && [pawnSquare] == Black::PAWN)
			return true;
		return false;
	}

	bool TryCastleShort() {
		if (!_blackCastleShort || [f8] || [g8]) 
			return false;
		if (isThreatening<White>(e8) || isThreatening<White>(f8) || isThreatening<White>(g8)) 
			return false;
		[f8] = Black::ROOK;
		[g8] = Black::KING;
		[h8] = nullpiece;
		[e8] = nullpiece;
		_blackCastleShort = false;
		_blackCastleLong = false;
		return true;
	}

	bool TryCastleLong() {
		if (!_blackCastleLong || [b8] || [c8] || [d8]) 
			return false;
		if (isThreatening<White>(e8) || isThreatening<White>(d8) || isThreatening<White>(c8)) 
			return false;
		[d8] = Black::ROOK;
		[c8] = Black::KING;
		[a8] = nullpiece;
		[e8] = nullpiece;
		_blackCastleShort = false;
		_blackCastleLong = false;
		return true;
	}

	void SetEnPassant(int fromSquare, int toSquare) {
		if ([toSquare] == Black::PAWN && Rank(fromSquare) == '7' && toSquare == (DOWN_DOWN + fromSquare)) {
			_enPassant = DOWN + fromSquare;
			return;
		}
		_enPassant = nullsquare;
	}

	void SetCastlingRights(int fromSquare, int toSquare) {
		switch (fromSquare) {
			case e8: 
				_blackCastleShort = false;
				_blackCastleLong = false;
				break;      
			case a8: 
				_blackCastleLong = false;
				break;      
			case h8: 
				_blackCastleShort = false;
				break;      
		}
		switch (toSquare) {
			case a1: 
				_whiteCastleLong = false;
				break;      
			case h1: 
				_whiteCastleShort = false;
				break;      
		}
	}


	static const int SHIFT = ('a' - 'A');
	static const char PAWN = Chess::PAWN + SHIFT;
	static const char KNIGHT = Chess::KNIGHT + SHIFT;
	static const char BISHOP = Chess::BISHOP + SHIFT;
	static const char ROOK = Chess::ROOK + SHIFT;
	static const char QUEEN = Chess::QUEEN + SHIFT;
	static const char KING = Chess::KING + SHIFT;

	static inline bool isMyPiece(char piece) {
		switch (piece) { case PAWN: case KNIGHT: case BISHOP: case ROOK: case QUEEN: case KING: return true; }
		return false;
	}
	static inline bool isOppPiece(char piece) { return piece && !IsMyPiece(piece); }
	static inline char piece(char p) { return p - SHIFT; }
	static inline char myPiece(char p) { return p + SHIFT; }

	static const int PAWN_DIRECTION = CHESS::DOWN;
};
