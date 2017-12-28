
#pragma once

#include <stdio.h>
#include "Move.cpp"
#include <iostream>
#include <string>
#include <stack>


struct BoardState {

public:

    char squares[64];
    int clock; // ply
    bool whiteCastleK;
    bool whiteCastleQ;
    bool blackCastleK;
    bool blackCastleQ;
    int enPassant;

	BoardState() : clock(0),
			  whiteCastleK(true),
			  whiteCastleQ(true),
			  blackCastleK(true),
			  blackCastleQ(true),
			  enPassant(-1) 
	{
		std::string init =  "RNBQKBNRPPPPPPPP" + std::string(32, 0) +  "pppppppprnbqkbnr";
		memcpy(squares, init.data(), 64);
	}	  

    bool TryMoveWhite(Move move);
    bool TryMoveBlack(Move move);

    bool IsValid();

    inline bool WhiteToMove() const { return clock % 2 == 0; }
	std::string ToString() const;	

private:

    static int NewSquare(Move move) {
        return (move.toRank - '1') * 8 + move.toFile - 'a';
    }

	int FindOriginalSquare(char piece, int toSquare, Move move);
    bool IsLegalMove(int orginalSquare, int toSquare, Move move);

	void SetEnPassantSquareWhite(int fromSquare, int toSquare, Move move);
	void SetEnPassantSquareBlack(int fromSquare, int toSquare, Move move);

	void SetCastleRightsWhite(int fromSquare, int toSquare);
	void SetCastleRightsBlack(int fromSquare, int toSquare);

    bool BlockTest(int fromSquare, int toSquare);
	bool IsThreatenedByBlack(int square);
	bool IsThreatenedByWhite(int square);

	bool IsWhitePiece(char piece) {
		switch (piece) {
			case 'P':
			case 'N':
			case 'B':
			case 'R':
			case 'Q':
			case 'K':
				return true;
		}
		return false;
	}

	bool IsBlackPiece(char piece) {
		switch (piece) {
			case 'p':
			case 'n':
			case 'b':
			case 'r':
			case 'q':
			case 'k':
				return true;
		}
		return false;
	}

    enum Square {a1, b1, c1, d1, e1, f1, g1, h1,
				 a2, b2, c2, d2, e2, f2, g2, h2,
				 a3, b3, c3, d3, e3, f3, g3, h3,
				 a4, b4, c4, d4, e4, f4, g4, h4,
				 a5, b5, c5, d5, e5, f5, g5, h5,
				 a6, b6, c6, d6, e6, f6, g6, h6,
				 a7, b7, c7, d7, e7, f7, g7, h7,
				 a8, b8, c8, d8, e8, f8, g8, h8
	};
};

class Board {

public:
	Board() : _state(new BoardState()) {}
	Board(const Board& rhs) : _state(new BoardState(*rhs._state)) {}

	virtual bool TryMove(const std::string& m) {
	
		Move move(m);

		if (!move.isValid) {
			std::cerr << m << ": Invalid Move\n";
			return false;
		}
		std::cerr << m << ": Valid Move\n";

		return WhiteToMove() ?
			   _state->TryMoveWhite(move) :
			   _state->TryMoveBlack(move);
	}

	bool WhiteToMove() const { return _state->WhiteToMove(); }
	int clock() const { return _state->clock; }
	std::string ToString() const { return _state->ToString(); }
	const char* data() const { return _state->squares; }
	char operator [](int i) const { return _state->squares[i]; }


protected:
	BoardState* _state;
};


class MemoryBoard : public Board {
	
public:

    virtual bool TryMove(const std::string& move) override {

		_history.push(_state);
		_state = new BoardState(*_state);

		if (!Board::TryMove(move)) {
			delete _state;
			_state = _history.top();
			_history.pop();
			return false;
		}	
		
		while (!_future.empty()) {
			delete _future.top();
			_future.pop();
		}

		_state->clock++;
		return true;
	}			

    void UndoMove();
    void RedoMove();

    
protected:
	std::stack<BoardState*> _history;
	std::stack<BoardState*> _future;
};

