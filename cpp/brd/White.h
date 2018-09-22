

#pragma once

#include "Chess.h"
#include "BoardState.h"

class White  : public BoardState {

public:
	bool isThreateningWithPawn(int square) const {
		auto pawnSquare = DOWN_RIGHT + square;
		if (IsSquare(pawnSquare) && [pawnSquare] == PAWN)
			return true;
		pawnSquare = DOWN_LEFT + square;
		if (IsSquare(pawnSquare) && [pawnSquare] == PAWN)
			return true;
		return false;
	}

	bool TryCastleShort() {
		if (!_whiteCastleShort || [f1] || [g1]) 
			return false;
		if (isThreatening<Black>(e1) || isThreatening<Black>(f1) || isThreatening<Black>(g1)) 
			return false;
		[f1] = ROOK;
		[g1] = KING;
		[h1] = nullpiece;
		[e1] = nullpiece;
		_whiteCastleShort = false;
		_whiteCastleLong = false;
		return true;
	}

	bool TryCastleLong() {
		if (!_whiteCastleLong || [b1] || [c1] || [d1]) 
			return false;
		if (isThreatening<Black>(e1) || isThreatening<Black>(d1) || isThreatening<Black>(c1)) 
			return false;
		[d1] = ROOK;
		[c1] = KING;
		[a1] = nullpiece;
		[e1] = nullpiece;
		_whiteCastleShort = false;
		_whiteCastleLong = false;
		return true;
	}

	void SetEnPassant(int fromSquare, int toSquare) {
		if ([toSquare] == PAWN && Rank(fromSquare) == '2' && toSquare == (UP_UP + fromSquare)) {
			_enPassant = UP + fromSquare;
			return;
		}
		_enPassant = nullsquare;
	}

	void SetCastlingRights(int fromSquare, int toSquare) {
		switch (fromSquare) {
			case e1: 
				SetWhiteCastleShort(false);
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


	static const char PAWN = Chess::PAWN;
	static const char KNIGHT = Chess::KNIGHT;
	static const char BISHOP = Chess::BISHOP;
	static const char ROOK = Chess::ROOK;
	static const char QUEEN = Chess::QUEEN;
	static const char KING = Chess::KING;

	static inline bool isMyPiece(char piece) {
		switch (piece) { case PAWN: case KNIGHT: case BISHOP: case ROOK: case QUEEN: case KING: return true; }
		return false;
	}
	static inline bool isOppPiece(char piece) { return piece && !IsMyPiece(piece); }
	static inline char piece(char p) { return p; }
	static inline char myPiece(char p) { return p; }

	static const int PAWN_DIRECTION = CHESS::UP;
};

