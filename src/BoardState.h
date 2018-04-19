
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
		int newSquare = newSquare(move);

		if (color::isMyPiece([newSquare])) return false;

		if (!move.piece() && !tryMatchPiece<color>(move)) return false;

		if (move.isCastleShort())
			return static_cast<color*>(this)->TryCastleShort();
		if (move.isCastleLong())
			return static_cast<color*>(this)->TryCastleLong();
		
		if (!isSquare(oldSquare(move)) && !tryMatchOldSquare<color>(move)) return false;
		int oldSquare = oldSquare(move);

		[newSquare] = [oldSquare];
		[oldSquare] = nullpiece;

		if (move.promotion())
			[newSquare] = color::myPiece(move.promotion());

		if (move.checkMate() && !isCheckMated<color::opposite>()) return false;
		if (move.check() && !isInCheck<color::opposite>()) return false;

		static_cast<color*>(this)->SetEnPassant(oldSquare, newSquare);
		static_cast<color*>(this)->SetCastlingRights(oldSquare, newSquare);

		return true;
	}

	inline char operator[](int i) const { return _squares[i]; } 
	inline bool clock() const { return _clock; }
	inline std::string enPassant() const { return Chess::toString(_enPassant); }
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
	bool IsThreateningWithPiece(int square) { 
	if (isThere<UP_UP_RIGHT, color::KNIGHT>(square)) return true;
	if (isThere<UP_RIGHT_RIGHT, color::KNIGHT>(square)) return true;
	if (isThere<DOWN_RIGHT_RIGHT, color::KNIGHT>(square)) return true;
	if (isThere<DOWN_DOWN_RIGHT, color::KNIGHT>(square)) return true;
	if (isThere<DOWN_DOWN_LEFT, color::KNIGHT>(square)) return true;
	if (isThere<DOWN_LEFT_LEFT, color::KNIGHT>(square)) return true;
	if (isThere<UP_LEFT_LEFT, color::KNIGHT>(square)) return true;
	if (isThere<UP_UP_LEFT, color::KNIGHT>(square)) return true;
	if (isFirst<UP, color::ROOK, color::QUEEN>(square)) return true;
	if (isFirst<RIGHT, color::ROOK, color::QUEEN>(square)) return true;
	if (isFirst<DOWN, color::ROOK, color::QUEEN>(square)) return true;
	if (isFirst<LEFT, color::ROOK, color::QUEEN>(square)) return true;
	if (isFirst<UP_RIGHT, color::ROOK, color::QUEEN>(square)) return true;
	if (isFirst<DOWN_RIGHT, color::ROOK, color::QUEEN>(square)) return true;
	if (isFirst<DOWN_LEFT, color::ROOK, color::QUEEN>(square)) return true;
	if (isFirst<UP_LEFT, color::ROOK, color::QUEEN>(square)) return true;
	return false;
	}

	template<typename color>
	bool IsThreatening(int square) {
		return static_cast<color*>(this)->IsThreateningWithPawn(square) || IsThreateningWithPiece<color>(square);
	}

	template <int direction, char piece0, char piece1>
	bool isFirst(int square) {
		for (int i = square + direction; isSquare(i); i = i + direction) {
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
			return true;
		for (int i = oldSquare + direction; i + direction; i != newSquare - direction) {
			if ([i]) return false;
		}
		return true;
	}

	template<typename color>
	int findKing() const {
		int i = NUM_SQUARES - 1;
		while(i >= 0 && _squares[i] != color::KING) i--;
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
			if (isLegalMove<color>(i, move)) {
				if (isSquare(oldSquare))
					return nullsquare;
				oldSquare = i;
			}
		}
		return oldSquare;
	}

	template<typename color>
	bool isLegalMove(int oldSquare, Move& move) {
		if ([oldSquare] != color::myPiece(move.piece()))
			return false;
		if (!move.TryMatchOldSquare(oldSquare))
			return false;
		if ([oldSquare] == Chess:PAWN) {
			if (!pawnDirectionTest<color>(move))
				return false;
			if (!pawnCaptureTest<color>(move))
				return false;
		}
		if (move.takes() && ![newSquare]) return false;
		int newSquare = newSquare(move);
		return !isBlocked(oldSquare, newSquare) && isInCheck<color>(oldSquare, newSquare);
	}

	template<typename color>
	bool pawnCaptureTest(oldSquare, newSquare) {
		if (_enPassant == newSquare) {
			[color::opposite::PAWN_DIRECTION + _enPassant] = nullpiece;
			[_enPassant] = color::opposite::PAWN;
		}
	}

	template<typename color>
	bool pawnDirectionTest(Move move) {
		bool isMovingUp = move.newRank() - move.oldRank() > 0;
		bool shouldMoveUp = (color::PAWN_DIRECTION == Chess::UP);
		if (isMovingUp != shouldMoveUp)
			return false;
		return true;
	}

};



