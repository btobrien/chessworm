
#pragma once

#include "chess/include/squares.h"
#include "chess/include/pieces.h"
#include "chess/include/castle_flags.h"
#include "move.h"
#include <string>

#include <iostream>
using std::cerr;
using std::endl;

class White;
class Black;

class BoardState {

public:
	BoardState() : _clock(0),
				   _enPassant(nullsquare),
				   _flags()
	{
		// static initialization? 
		std::string init =  "RNBQKBNRPPPPPPPP" + std::string(NUM_SQUARES/2, Chess::nullpiece) +  "pppppppprnbqkbnr";
		memcpy(_, init.data(), NUM_SQUARES);
	}	  

	BoardState(const BoardState& other) : _clock(other._clock),
										  _enPassant(other._enPassant),
										  _flags(other._flags)
	{ memcpy(_, other._, NUM_SQUARES); }	  

	BoardState(const std::string& squares, CastleFlags flags, int clock, int en_passant) : _clock(clock),
																						  _enPassant(en_passant),
																						  _flags(flags)
	{ memcpy(_, squares.data(), NUM_SQUARES); }	  

	// TODO assignment operator ...?


	template <typename color>
	bool TryMove(Move move) {
		_clock++;

		int newSquare = move.newSquare();
		if (color::isPiece(_[newSquare])) {
			cerr << "ERROR: cannot capture own piece" << endl;
			return false;
		}

		if (move.castleShort())
			return static_cast<color*>(this)->TryCastleShort();
		if (move.castleLong())
			return static_cast<color*>(this)->TryCastleLong();
		
		int oldSquare = tryFindOldSquare<color>(move);
		if (!isSquare(oldSquare)) {
			cerr << "ERROR: cannot locate piece" << endl;
			return false;
		}

		if (isEnPassantCapture<color>(oldSquare, newSquare))
			_[_enPassant - color::PAWN_DIRECTION] = Chess::nullpiece;
		else if (move.takes() && !_[newSquare]) {
			cerr << "WARN: capture square is empty" << endl;
		}

		_[newSquare] = _[oldSquare];
		_[oldSquare] = Chess::nullpiece;

		if (move.promotion())
			_[newSquare] = color::piece(move.promotion());

		//if (move.checkMate() && !isMated<color>()) WARN
		//if (move.check() && !isChecked<color>()) WARN

		static_cast<color*>(this)->SetEnPassant(oldSquare, newSquare);
		static_cast<color*>(this)->SetCastlingRights(oldSquare, newSquare);

		return true;
	}

	inline char operator[](int i) const { return _[i]; } 
	inline int clock() const { return _clock; }
	inline int enPassant() const { return _enPassant; }
	inline CastleFlags flags() const { return _flags; }

private:
    char _[NUM_SQUARES];
	int  _clock;
    int  _enPassant;
	CastleFlags _flags;

	friend class White;
	friend class Black;

	template<typename color>
	bool isChecked() const {
		return static_cast<const color*>(this)->isThreatened(findKing<color>());
	}

	template<typename color>
	bool isEnPassantCapture(int oldSquare, int newSquare) const {
		return _[oldSquare] == color::piece(Chess::PAWN) && newSquare == _enPassant;
	}

	template<typename color>
	bool isThreateningWithKnight(int square) const { 
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
	bool isThreateningFromSide(int square) const { 
		if (isUnblocked<UP, color::ROOK, color::QUEEN>(square)) return true;
		if (isUnblocked<RIGHT, color::ROOK, color::QUEEN>(square)) return true;
		if (isUnblocked<DOWN, color::ROOK, color::QUEEN>(square)) return true;
		if (isUnblocked<LEFT, color::ROOK, color::QUEEN>(square)) return true;
		return false;
	}

	template<typename color>
	bool isThreateningFromDiag(int square) const { 
		if (isUnblocked<UP_RIGHT, color::BISHOP, color::QUEEN>(square)) return true;
		if (isUnblocked<DOWN_RIGHT, color::BISHOP, color::QUEEN>(square)) return true;
		if (isUnblocked<DOWN_LEFT, color::BISHOP, color::QUEEN>(square)) return true;
		if (isUnblocked<UP_LEFT, color::BISHOP, color::QUEEN>(square)) return true;
		return false;
	}

	template<typename color>
	bool isThreatening(int square) const {
		if (static_cast<const color*>(this)->isThreateningWithPawn(square)) return true;
		if (isThreateningWithKnight<color>(square)) return true;
		if (isThreateningFromSide<color>(square)) return true;
		if (isThreateningFromDiag<color>(square)) return true;
		return false;
	}

	template <int direction, char piece0, char piece1>
	bool isUnblocked(int square) const {
		for (int i = square + direction; isSquare(i); i += direction) {
			if (_[i])
				return (_[i] == piece0 || _[i] == piece1);
		}
		return false;
	}

	template <int direction, char piece>
	bool isThere(int square) const {
		int i = square + direction;
		return isSquare(i) ? _[i] == piece : false;
	}

	bool isBlocked(char oldSquare, char newSquare) const {
		int direction = lineDirection(oldSquare, newSquare);
		if (!direction)
			return false;
		for (int i = oldSquare + direction; i != newSquare; i += direction) {
			if (_[i]) return true;
		}
		return false;
	}

	template<typename color>
	int findKing() const {
		int i = NUM_SQUARES - 1;
		while(i >= 0 && _[i] != color::KING)
			i--;
		return i;
	}

	template<typename color>
	bool isChecked(int oldSquare, int newSquare) {
		char capturingPiece = _[oldSquare];
		char capturedPiece = _[newSquare];	
		_[oldSquare] = Chess::nullpiece;
		_[newSquare] = capturingPiece;
		bool result = isChecked<color>();
		_[newSquare] = capturedPiece;
		_[oldSquare] = capturingPiece;
		return result;
	}

	template<typename color>
	int tryFindOldSquare(Move& move) {
		int oldSquare = nullsquare;
		for (int i = 0; i < NUM_SQUARES; i++) {
			if (isLegalMove<color>(move, i)) {
				if (isSquare(oldSquare)) {
					cerr << "ERROR: ambigous move" << endl;
					return nullsquare;
				}
				oldSquare = i;
			}
		}
		return oldSquare;
	}

	template<typename color>
	bool isLegalMove(Move& move, int oldSquare) {
		char piece = color::whichPiece(_[oldSquare]);
		if (!move.tryMatch(piece, oldSquare))
			return false;
		int newSquare = move.newSquare();
		if (piece == Chess::PAWN && !isRightPawnDirection<color>(oldSquare, newSquare))
			return false;
		if (piece == Chess::PAWN && _[newSquare])
			return false;
		return !isBlocked(oldSquare, newSquare) && !isChecked<color>(oldSquare, newSquare);
	}

	template<typename color>
	bool isRightPawnDirection(int oldSquare, int newSquare) const {
		bool isMovingUp = (rank(newSquare) - rank(oldSquare)) > 0;
		bool shouldMoveUp = (color::PAWN_DIRECTION > 0);
		return isMovingUp == shouldMoveUp;
	}

};



