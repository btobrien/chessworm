
#pragma once

#include "Chess.h"
#include "BoardState.h"

class White  : public BoardState {
public:
	bool isThreatened(int square) const;
	bool TryCastleShort();
	bool TryCastleLong();
	void SetEnPassant(int fromSquare, int toSquare);
	void SetCastlingRights(int fromSquare, int toSquare);

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

	static const int PAWN_DIRECTION = CHESS::UP;
};

namespace Black {

class BoardState : public Impl::BoardState {
public:
	bool isThreatened(int square) const;
	bool TryCastleShort();
	bool TryCastleLong();
	void SetEnPassant(int fromSquare, int toSquare);
	void SetCastlingRights(int fromSquare, int toSquare);

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

	static const int PAWN_DIRECTION = CHESS::DOWN;
};
