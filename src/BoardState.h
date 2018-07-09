
#pragma once

#include <stdio.h>
#include "Move.h"
#include "Log.h"
#include <iostream>
#include <string>


class BoardState {

public:
	BoardState() : clock(0),
				   enPassant(nullsquare)
				   whiteCastleShort(true),
				   whiteCastleLong(true),
				   blackCastleShort(true),
				   blackCastleLong(true)
	{
		// static initialization? 
		std::string init =  "RNBQKBNRPPPPPPPP" + std::string(NUM_SQUARES / 2, 0) +  "pppppppprnbqkbnr";
		memcpy(squares, init.data(), NUM_SQUARES);
	}	  

	BoardState(const BoardState& other) : clock(other.clock),
										  enPassant(other.enPassant),
										  whiteCastleShort(other.whiteCastleShort),
										  whiteCastleLong(other.whiteCastleLong),
										  blackCastleShort(other.blackCastleShort),
										  blackCastleLong(other.blackCastleLong)
	{ memcpy(squares, other.squares, NUM_SQUARES); }	  


	template <typename color>
	bool TryMove(Move move) {
		_clock++;

		int newSquare = move.newSquare();

		if (color::isMyPiece([newSquare])) return false;

		if (move.isCastleShort())
			return static_cast<color*>(this)->TryCastleShort();
		if (move.isCastleLong())
			return static_cast<color*>(this)->TryCastleLong();
		
		int oldSquare = tryFindOldSquare<color>(move)
		if (!isSquare(oldSquare)) return false

		if (isEnPassantCapture<color>(oldSquare, newSquare))
			[color::opposite::PAWN_DIRECTION + _enPassant] = nullpiece;
		else if (move.takes() && ![newSquare]) return false;

		[newSquare] = [oldSquare];
		[oldSquare] = nullpiece;

		if (move.promotion())
			[newSquare] = color::myPiece(move.promotion());

		//if (move.checkMate() && !isCheckMated<color::opposite>()) return false;
		if (move.check() && !isInCheck<color::opposite>()) return false;

		static_cast<color*>(this)->SetEnPassant(oldSquare, newSquare);
		static_cast<color*>(this)->SetCastlingRights(oldSquare, newSquare);

		return true;
	}

	inline char operator[](int i) const { return _squares[i]; } 
	inline int clock() const { return _clock; }
	inline int enPassant() const { return _enPassant; }
	inline bool whiteCastleShort() const { return _whiteCastleShort; }
	inline bool whiteCastleLong()  const { return _whiteCastleLong; }
	inline bool blackCastleShort() const { return _blackCastleShort; }
	inline bool blackCastleLong()  const { return _blackCastleLong; }

	std::string key() const {
		string key = Chess::fen(*this);
		Logger::log("BoardState::key returning " + key); //chop off end
		return key;
	}

	template<typename color>
	bool isInCheck() const {
		return !isThreatening<color::opposite>(findKing<color>());
	}

	template <typename color>
	bool isCheckMated() {
		// dummy impl
		return isInCheck<color>();
	}

private:
    char _squares[NUM_SQUARES];
	inline char& operator[](int i) { return _squares[i]; } 
	int  _clock;
    int  _enPassant;
	bool _whiteCastleShort;
	bool _whiteCastleLong;
	bool _blackCastleShort;
	bool _blackCastleLong;

	friend White;
	friend Black;

	template<typename color>
	bool isEnPassantCapture(int oldSquare, int newSquare) {
		return color::piece([oldSquare]) == PAWN && newSquare == _enPassant;
	}

	template<typename color>
	bool isThreatentingWithKnight(int square) { 
		if (isThere<UP_UP_RIGHT, color::KNIGHT>(square)) return true;
		if (isThere<UP_RIGHT_RIGHT, color::KNIGHT>(square)) return true;
		if (isThere<DOWN_RIGHT_RIGHT, color::KNIGHT>(square)) return true;
		if (isThere<DOWN_DOWN_RIGHT, color::KNIGHT>(square)) return true;
		if (isThere<DOWN_DOWN_LEFT, color::KNIGHT>(square)) return true;
		if (isThere<DOWN_LEFT_LEFT, color::KNIGHT>(square)) return true;
		if (isThere<UP_LEFT_LEFT, color::KNIGHT>(square)) return true;
		if (isThere<UP_UP_LEFT, color::KNIGHT>(square)) return true;
		return false;
	}

	template<typename color>
	bool isThreateningFromSide(int square) { 
		if (isFirst<UP, color::ROOK, color::QUEEN>(square)) return true;
		if (isFirst<RIGHT, color::ROOK, color::QUEEN>(square)) return true;
		if (isFirst<DOWN, color::ROOK, color::QUEEN>(square)) return true;
		if (isFirst<LEFT, color::ROOK, color::QUEEN>(square)) return true;
		return false;
	}

	template<typename color>
	bool isThreateningFromDiag(int square) { 
		if (isFirst<UP_RIGHT, color::BISHOP, color::QUEEN>(square)) return true;
		if (isFirst<DOWN_RIGHT, color::BISHOP, color::QUEEN>(square)) return true;
		if (isFirst<DOWN_LEFT, color::BISHOP, color::QUEEN>(square)) return true;
		if (isFirst<UP_LEFT, color::BISHOP, color::QUEEN>(square)) return true;
		return false;
	}

	template<typename color>
	bool isThreatening(int square) {
		if (static_cast<color*>(this)->IsThreateningWithPawn(square)) return true;
		if isThreatentingWithKnight<color>(square) return true;
		if isThreateningFromSide<color>(square) return true;
		if isThreateningFromDiag<color>(square) return true;
		return false;
	}

	template <int direction, char piece0, char piece1>
	bool isFirst(int square) {
		for (int i = square + direction; isSquare(i); i += direction) {
			if ([i])
				return ([i] == piece0 || [i] == piece1);
		}
		return false;
	}


	template <int direction, char piece>
	bool isThere(int square);
		int i = square + direction;
		return isSquare(i) ? [i] == piece : false;
	}

	bool isBlocked(char oldSquare, char newSquare) {
		int direction = lineDirection(oldSquare, newSquare);
		if (!direction)
			return false;
		for (int i = oldSquare + direction; i != (newSquare - direction); i += direction) {
			if ([i]) return true;
		}
		return false;
	}

	template<typename color>
	int findKing() const {
		int i = NUM_SQUARES - 1;
		while(i >= 0 && [i] != color::KING)
			i--;
		return i;
	}


	template<typename color>
	bool isInCheck(int oldSquare, int newSquare) {

		char capturingPiece = [oldSquare];
		char capturedPiece = [newSquare];	

		[oldSquare] = nullpiece;
		[newSquare] = capturingPiece;
		
		bool result = isInCheck<color>();

		[newSquare] = capturedPiece;
		[oldSquare] = capturingPiece;

		return result;
	}


	template<typename color>
	int tryFindOldSquare(Move& move) {
		int oldSquare = nullsquare;
		for (int i = 0; i < NUM_SQUARES; i++) {
			if (isLegalMove<color>(move, i)) {
				if (isSquare(oldSquare))
					return nullsquare;
				oldSquare = i;
			}
		}
		return oldSquare;
	}

	template<typename color>
	bool isLegalMove(Move& move, int oldSquare) {
		char piece = color::piece([oldSquare])
		if (!move.tryMatch(piece, oldSquare))
			return false;
		int newSquare = move.newSquare();
		if (piece == Chess:PAWN && !isRightPawnDirection<color>(oldSquare, newSquare)
			return false;
		return !isBlocked(oldSquare, newSquare) && !isInCheck<color>(oldSquare, newSquare);
	}

	template<typename color>
	bool isRightPawnDirection(int oldSquare, int newSquare) {
		bool isMovingUp = (rank(oldSquare) - rank(newSquare)) > 0;
		bool shouldMoveUp = (color::PAWN_DIRECTION > 0);
		return isMovingUp == shouldMoveUp;
	}

};



