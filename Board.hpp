
#pragma once

#include <stdio.h>
#include "Move.cpp"
#include <iostream>
#include <string>
#include <stack>


struct BoardState {

public:

    char squares[64];
    int clock; // num of half moves made so far
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
		std::string init =  "RNBQKBNRPPPPPPPP" + std::string(16, 0) +  "pppppppprnbqkbnr";
		memcpy(squares, init.data(), 64);
	}	  

    bool TryMoveWhite(const Move& move);
    bool TryMoveBlack(const Move& move);

    bool IsValid();

    inline bool WhiteToMove() const { return clock % 2 == 0; }
	std::string ToString() const;	

private:

    static int NewSquare(const Move& move) {
        return (move.toRank - '1') * 8 + move.toFile - 'a';
    }

	int FindOriginalSquare(char piece, int newSquare, const Move& move);
    bool IsLegalMove(int orginalSquare, int newSquare, const Move& move);

	void SetEnPassantSquareWhite(int originalSquare, int newSquare, const Move& move);
	void SetEnPassantSquareBlack(int originalSquare, int newSquare, const Move& move);

	void SetCastleRightsWhite(int originalSquare, int newSquare);
	void SetCastleRightsBlack(int originalSquare, int newSquare);

    bool BlockTest(int originaSquare, int newSquare);
	bool IsThreatened(int square);

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
		if (!move.isValid)
			return false;

		return WhiteToMove() ?
			   _state->TryMoveWhite(move) :
			   _state->TryMoveBlack(move);
	}

	virtual bool WhiteToMove() const { return _state->WhiteToMove(); }
	virtual int clock() const { return _state->clock; }
	virtual std::string ToString() const { return _state->ToString(); }
	virtual const char* data() const { return _state->squares; }

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

