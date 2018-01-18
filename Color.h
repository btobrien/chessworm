
#pragma once

#include "Move.cpp"
#include "BoardState.h"

class White : public BoardState {
public:
	static const char PAWN = Move::PAWN;
	static const char KNIGHT = Move::KNIGHT;
	static const char BISHOP = Move::BISHOP;
	static const char ROOK = Move::ROOK;
	static const char QUEEN = Move::QUEEN;
	static const char KING = Move::KING;

	static inline bool IsMyPiece(char piece) {
		switch (piece) { case PAWN: case KNIGHT: case BISHOP: case ROOK: case QUEEN: case KING: return true; }
		return false;
	}

	static inline bool IsOppPiece(char piece) { return piece && !IsMyPiece(piece); }
	static inline char Piece(Move move) { return move.piece; }
	static inline char Piece(char piece) { return piece; }
	static inline bool DoPawnsMoveUp() { return true; }

	bool IsThreatened(int square);
	bool TryCastleShort();
	bool TryCastleLong();
	void CaptureEnPassant();
	void SetEnPassant(int fromSquare, int toSquare);
	void SetCastlingRights(int fromSquare, int toSquare);
};

class Black : public BoardState {
private: 
	static const int SHIFT = ('a' - 'A');
public:
	static const char PAWN = Move::PAWN + SHIFT;
	static const char KNIGHT = Move::KNIGHT + SHIFT;
	static const char BISHOP = Move::BISHOP + SHIFT;
	static const char ROOK = Move::ROOK + SHIFT;
	static const char QUEEN = Move::QUEEN + SHIFT;
	static const char KING = Move::KING + SHIFT;

	static inline bool IsMyPiece(char piece) {
		switch (piece) { case PAWN: case KNIGHT: case BISHOP: case ROOK: case QUEEN: case KING: return true; }
		return false;
	}

	static inline bool IsOppPiece(char piece) { return piece && !IsMyPiece(piece); }
	static inline char Piece(Move move) { return move.piece + SHIFT; }
	static inline char Piece(char piece) { return piece + SHIFT; }
	static inline bool DoPawnsMoveUp() { return false; }

	bool IsThreatened(int square);
	bool TryCastleShort();
	bool TryCastleLong();
	void CaptureEnPassant();
	void SetEnPassant(int fromSquare, int toSquare);
	void SetCastlingRights(int fromSquare, int toSquare);
};
