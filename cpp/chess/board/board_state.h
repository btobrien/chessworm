
#pragma once

#include <string>
#include "chess/include/squares.h"
#include "chess/include/pieces.h"
#include "move.h"

class White;
class Black;

class BoardState {

public:
	BoardState() : _clock(0),
				   _enPassant(nullsquare),
				   _whiteCastleShort(true),
				   _whiteCastleLong(true),
				   _blackCastleShort(true),
				   _blackCastleLong(true)
	{
		// static initialization? 
		std::string init =  "RNBQKBNRPPPPPPPP" + std::string(NUM_SQUARES / 2, 0) +  "pppppppprnbqkbnr";
		memcpy(_, init.data(), NUM_SQUARES);
	}	  

	BoardState(const BoardState& other) : _clock(other._clock),
										  _enPassant(other._enPassant),
										  _whiteCastleShort(other._whiteCastleShort),
										  _whiteCastleLong(other._whiteCastleLong),
										  _blackCastleShort(other._blackCastleShort),
										  _blackCastleLong(other._blackCastleLong)
	{ memcpy(_, other._, NUM_SQUARES); }	  


	template <typename color>
	bool TryMove(Move move) {
		_clock++;

		int newSquare = move.newSquare();
		if (color::isMyPiece(_[newSquare])) return false;

		if (move.castleShort())
			return static_cast<color*>(this)->TryCastleShort();
		if (move.castleLong())
			return static_cast<color*>(this)->TryCastleLong();
		
		int oldSquare = tryFindOldSquare<color>(move);
		if (!isSquare(oldSquare)) return false;

		if (isEnPassantCapture<color>(oldSquare, newSquare))
			_[_enPassant - color::PAWN_DIRECTION] = Chess::nullpiece;
		else if (move.takes() && !_[newSquare]) return false;

		_[newSquare] = _[oldSquare];
		_[oldSquare] = Chess::nullpiece;

		if (move.promotion())
			_[newSquare] = color::myPiece(move.promotion());

		//if (move.checkMate() && !isMated<color>()) return false;
		//if (move.check() && !isChecked<color>()) return false;

		static_cast<color*>(this)->SetEnPassant(oldSquare, newSquare);
		static_cast<color*>(this)->SetCastlingRights(oldSquare, newSquare);

		return true;
	}

	inline char operator[](int i) const { return _[i]; } 
	inline int clock() const { return _clock; }
	inline int enPassant() const { return _enPassant; }
	inline bool whiteCastleShort() const { return _whiteCastleShort; }
	inline bool whiteCastleLong()  const { return _whiteCastleLong; }
	inline bool blackCastleShort() const { return _blackCastleShort; }
	inline bool blackCastleLong()  const { return _blackCastleLong; }

	template<typename color>
	bool isChecked() const {
		return static_cast<const color*>(this)->isThreatened(findKing<color>());
	}

private:
    char _[NUM_SQUARES];
	int  _clock;
    int  _enPassant;
	bool _whiteCastleShort;
	bool _whiteCastleLong;
	bool _blackCastleShort;
	bool _blackCastleLong;

	friend class White;
	friend class Black;

	template<typename color>
	bool isEnPassantCapture(int oldSquare, int newSquare) const {
		return _[oldSquare] == color::myPiece(Chess::PAWN) && newSquare == _enPassant;
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
		if (isFirst<UP, color::ROOK, color::QUEEN>(square)) return true;
		if (isFirst<RIGHT, color::ROOK, color::QUEEN>(square)) return true;
		if (isFirst<DOWN, color::ROOK, color::QUEEN>(square)) return true;
		if (isFirst<LEFT, color::ROOK, color::QUEEN>(square)) return true;
		return false;
	}

	template<typename color>
	bool isThreateningFromDiag(int square) const { 
		if (isFirst<UP_RIGHT, color::BISHOP, color::QUEEN>(square)) return true;
		if (isFirst<DOWN_RIGHT, color::BISHOP, color::QUEEN>(square)) return true;
		if (isFirst<DOWN_LEFT, color::BISHOP, color::QUEEN>(square)) return true;
		if (isFirst<UP_LEFT, color::BISHOP, color::QUEEN>(square)) return true;
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
	bool isFirst(int square) const {
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
		for (int i = oldSquare + direction; i != (newSquare - direction); i += direction) {
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
				if (isSquare(oldSquare))
					return nullsquare;
				oldSquare = i;
			}
		}
		return oldSquare;
	}

	template<typename color>
	bool isLegalMove(Move& move, int oldSquare) {
		char piece = color::myPiece(_[oldSquare]);
		if (!move.tryMatch(piece, oldSquare))
			return false;
		int newSquare = move.newSquare();
		if (piece == Chess::PAWN && !isRightPawnDirection<color>(oldSquare, newSquare))
			return false;
		return !isBlocked(oldSquare, newSquare) && !isChecked<color>(oldSquare, newSquare);
	}

	template<typename color>
	bool isRightPawnDirection(int oldSquare, int newSquare) const {
		bool isMovingUp = (rank(oldSquare) - rank(newSquare)) > 0;
		bool shouldMoveUp = (color::PAWN_DIRECTION > 0);
		return isMovingUp == shouldMoveUp;
	}

};



