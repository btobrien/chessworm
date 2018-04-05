
#pragma once

#include <stdio.h>
#include "Move.cpp"
#include "Log.h"
#include <iostream>
#include <string>

class BoardState {

public:
	static const int NUM_SQUARES = 64;
	static const char nullpiece = 0; // MUST BE 0!

    char squares[NUM_SQUARES];
    bool whiteCastleK;
    bool whiteCastleQ;
    bool blackCastleK;
    bool blackCastleQ;
    int  enPassant;
	int  clock;

	BoardState();
	BoardState(const BoardState& other);

	template <class color>
	bool TryMove(Move move) {

		clock++;
		int toSquare = Square(move);

		if (move.check && !CheckTest<color>())
			return false;

		if (move.castleShort)
			return static_cast<color*>(this)->TryCastleShort();
		if (move.castleLong)
			return static_cast<color*>(this)->TryCastleLong();
		
		if (static_cast<color*>(this)->IsMyPiece(squares[toSquare]))
			return false;

		if (move.piece == Move::PAWN && enPassant == toSquare) {
			move.takes = true;
			static_cast<color*>(this)->CaptureEnPassant();
		}
		else if (move.takes && !squares[toSquare])
			return false;
		else if (static_cast<color*>(this)->IsOppPiece(squares[toSquare]))
			move.takes = true;

		int fromSquare = TryFindAndMovePiece<color>(move);

		if (fromSquare == nullsquare)
			return false;

		if(move.promoted)
			squares[toSquare] = static_cast<color*>(this)->Piece(move.promoted);

		static_cast<color*>(this)->SetEnPassant(fromSquare, toSquare);
		static_cast<color*>(this)->SetCastlingRights(fromSquare, toSquare);
		
		return true;
	}

	std::string ToString() const;	

protected:
	static const char nullsquare = -1;

    static enum Square { a1, b1, c1, d1, e1, f1, g1, h1,
						 a2, b2, c2, d2, e2, f2, g2, h2,
						 a3, b3, c3, d3, e3, f3, g3, h3,
						 a4, b4, c4, d4, e4, f4, g4, h4,
						 a5, b5, c5, d5, e5, f5, g5, h5,
						 a6, b6, c6, d6, e6, f6, g6, h6,
						 a7, b7, c7, d7, e7, f7, g7, h7,
						 a8, b8, c8, d8, e8, f8, g8, h8 };

    inline static int Square(Move move) { return (move.toRank - '1') * 8 + move.toFile - 'a'; }
	inline bool IsSquare(int square) { return (0 <= square && square < NUM_SQUARES); }
	inline static char File(int square) { return (square % 8) + 'a'; }
	inline static char Rank(int square) { return (square / 8) + '1'; }

	static const int UP  = 8;
	static const int DOWN = -8;
	static const int RIGHT  = 1;
	static const int LEFT  = -1;

	typedef int (*Direction) (int);

	static inline int Up(int square) { return square + UP; }
	static inline int Down(int square) { return square + DOWN; }
	static inline int Right(int square) { return square + RIGHT; }
	static inline int Left(int square) { return square + LEFT; }

	static inline int UpRight(int square) { return Up(Right(square)); }
	static inline int DownRight(int square) { return Down(Right(square)); }
	static inline int DownLeft(int square) { return Down(Left(square)); }
	static inline int UpLeft(int square) { return Up(Left(square)); }

	static inline int UpRightKnight(int square) { return Up(Up(Right(square))); }
	static inline int DownRightKnight(int square) { return Down(Down(Right(square))); }
	static inline int DownLeftKnight(int square) { return Down(Down(Left(square))); }
	static inline int UpLeftKnight(int square) { return Up(Up(Left(square))); }

	static inline int RightUpKnight(int square) { return Right(Right(Up(square))); }
	static inline int RightDownKnight(int square) { return Right(Right(Down(square))); }
	static inline int LeftDownKnight(int square) { return Left(Left(Down(square))); }
	static inline int LeftUpKnight(int square) { return Left(Left(Up(square))); }

	template<char knight, char bishop, char rook, char queen, char king>
	bool IsThreatenedHelper(int square) { 
		if (IsNextInDirection(square, UpRightKnight, knight)) return true;
		if (IsNextInDirection(square, RightUpKnight, knight)) return true;
		if (IsNextInDirection(square, RightDownKnight, knight)) return true;
		if (IsNextInDirection(square, DownRightKnight, knight)) return true;
		if (IsNextInDirection(square, DownLeftKnight, knight)) return true;
		if (IsNextInDirection(square, LeftDownKnight, knight)) return true;
		if (IsNextInDirection(square, LeftUpKnight, knight)) return true;
		if (IsNextInDirection(square, UpLeftKnight, knight)) return true;
		if (IsFirstInDirection(square, Up, rook, queen)) return true;
		if (IsFirstInDirection(square, Right, rook, queen)) return true;
		if (IsFirstInDirection(square, Down, rook, queen)) return true;
		if (IsFirstInDirection(square, Left, rook, queen)) return true;
		if (IsFirstInDirection(square, UpRight, bishop, queen)) return true;
		if (IsFirstInDirection(square, DownRight, bishop, queen)) return true;
		if (IsFirstInDirection(square, DownLeft, bishop, queen)) return true;
		if (IsFirstInDirection(square, UpLeft, bishop, queen)) return true;
		return false;
	}

	bool IsFirstInDirection(int square, Direction direction, char piece0, char piece1);
	bool IsNextInDirection(int square, Direction direction, char piece);

	// REQUIRES: fromSquare and toSquare to be on the same file
	// EFFECTS: returns true iff all squares strictly between toSquare and fromSquare are empty
	bool IsFileOpen(char fromSquare, char toSquare);

	// REQUIRES: fromSquare and toSquare to be on the same rank
	// EFFECTS: returns true iff all squares strictly between toSquare and fromSquare are empty
	bool IsRankOpen(char fromSquare, char toSquare);

	// REQUIRES: fromSquare and toSquare to be on the same diagonal
	// EFFECTS: returns true iff all squares strictly between toSquare and fromSquare are empty
	bool IsDiagOpen(char fromSquare, char toSquare);

	template<class color>
	bool CheckTest() {

		int i = NUM_SQUARES - 1;
		while(i >= 0 && squares[i] != color::KING)
			i--;

		return !(static_cast<color*>(this)->IsThreatened(i));
	}

	template<class color>
	bool CheckTest(int fromSquare, int toSquare) {

		char capturingPiece = squares[fromSquare];
		char capturedPiece = squares[toSquare];	

		squares[fromSquare] = nullpiece;
		squares[toSquare] = capturingPiece;
		
		bool success = CheckTest<color>();

		squares[toSquare] = capturedPiece;
		squares[fromSquare] = capturingPiece;

		return success;
	}

	template<class color>
	int TryFindAndMovePiece(Move move) {

		char piece = color::Piece(move);
		int result = nullsquare;

		for (int i = 0; i < NUM_SQUARES; i++) {
			if (squares[i] == piece  && IsLegalMove<color>(i, move)) {
				if (IsSquare(result))
					return nullsquare;
				result = i;
			}
		}
		
		squares[result] = nullpiece;
		squares[Square(move)] = piece;

		return result;
	}

	template <class color>
	bool IsLegalMove(int fromSquare, Move move) {
		
		int toSquare = Square(move);

		if (fromSquare == toSquare)
			return false;

		char fromFile = File(fromSquare);
		char fromRank = Rank(fromSquare);
		
		if (move.fromFile && fromFile != move.fromFile)
			return false;
		if (move.fromRank && fromRank != move.fromRank)
			return false;
		
		int fileDistance = abs(fromFile - move.toFile);
		int rankDistance = abs(fromRank - move.toRank);
		int manhattanDistance = fileDistance + rankDistance;
		int manhattanDifference = abs(fileDistance - rankDistance); 
		bool isMovingUp = move.toRank - fromRank > 0;

		if (fileDistance && !rankDistance && !IsRankOpen(fromSquare, toSquare)) 
			return false;
		if (!fileDistance && rankDistance && !IsFileOpen(fromSquare, toSquare)) 
			return false;
		if (!manhattanDifference && !IsDiagOpen(fromSquare, toSquare)) 
			return false;

		switch (move.piece) {
			case Move::PAWN:
				if (!rankDistance)
					return false;
				if (isMovingUp != static_cast<color*>(this)->DoPawnsMoveUp())
					return false;
				if (move.takes) {
					if (manhattanDifference || manhattanDistance != 2)
						return false;
					break;
				}
				if (fileDistance)
					return false;
				if (rankDistance > 2)
					return false;
				if (fromRank != '2' && fromRank != '7' && rankDistance > 1)
					return false;
				break;
			case Move::KNIGHT:
				if (manhattanDistance != 3 || manhattanDifference != 1)
					return false;
				break;
			case Move::BISHOP: 
				if (manhattanDifference)
					return false;
				break;
			case Move::ROOK:
				if (rankDistance && fileDistance)
					return false;
				break;
			case Move::QUEEN:
				if (manhattanDifference && rankDistance && fileDistance)
					return false;
				break;
			case Move::KING:
				if (manhattanDifference && manhattanDistance != 1)
					return false;
				if (!manhattanDifference && manhattanDistance != 2)
					return false;
				break;
			default:
				return false;
		}

		return CheckTest<color>(fromSquare, toSquare);
	}
};



