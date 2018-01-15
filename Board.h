
#pragma once

#include <stdio.h>
#include "Move.cpp"
#include <iostream>
#include <string>
#include <stack>
#include <cassert>


struct BoardState {

	static char NULL_PIECE = -1;
	static char EMPTY_PIECE = 0;

	static char PAWN_W = 'P';
	static char PAWN_B = 'p';

	static char KNIGHT_W = 'K';
	static char KNIGHT_B = 'k';

	static char BISHOP_W = 'B';
	static char BISHOP_B = 'b';

	static char ROOK_W = 'R';
	static char ROOK_B = 'r';

	static char QUEEN_W = 'Q';
	static char QUEEN_B = 'q';

	static char KING_W = 'K';
	static char KING_B = 'k';

	static int NUM_SQUARES = 64; // ??

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
		// make static...AND use STATIC CONST VARS!!!!!!!!!!!!!
		std::string init =  "RNBQKBNRPPPPPPPP" + std::string(32, 0) +  "pppppppprnbqkbnr";
		memcpy(squares, init.data(), 64);
	}	  

	BoardState(const BoardState& other) : clock(other.clock),
			  whiteCastleK(other.whiteCastleK),
			  whiteCastleQ(other.whiteCastleQ),
			  blackCastleK(other.blackCastleK),
			  blackCastleQ(other.blackCastleQ),
			  enPassant(other.enPassant) 
	{
		memcpy(squares, other.squares, 64);
	}	  

	bool TryMove(Move move) {
		clock++;
		return WhiteToMove() ?
			   TryMoveWhite(move) :
			   TryMoveBlack(move);
	}

    inline bool WhiteToMove() const { return clock % 2 != 0; }
	std::string ToString() const;	

private:

    bool TryMoveWhite(Move move);
    bool TryMoveBlack(Move move);

	// Checks the safety of the king who's turn it is according to the clock
	bool CheckTest() {

		char king = WhiteToMove() ? 'K' : 'k';
		int i = 63;
		while(i >= 0 && squares[i] != king)
			i--;

		if (i < 0)
			assert(false);

		return WhiteToMove() ? !IsThreatenedByBlack(i) : !IsThreatenedByWhite(i);
	}

	bool CheckTest(int fromSquare, int toSquare) {

		char capturingPiece = squares[fromSquare];
		char capturedPiece = squares[toSquare];	

		squares[fromSquare] = 0;
		squares[toSquare] = capturingPiece;
		
		bool success = CheckTest();

		squares[toSquare] = capturedPiece;
		squares[fromSquare] = capturingPiece;

		return success;
	}

    static int Square(char file, char rank) { return (rank - '1') * 8 + file - 'a'; }
    static int Square(Move move) { return (move.toRank - '1') * 8 + move.toFile - 'a'; }
	static char File(int square) { return (square % 8) + 'a'; }
	static char Rank(int square) { return (square / 8) + '1'; }

	// REQUIRES: fromSquare and toSquare to be on the same file
	// EFFECTS: returns true iff all squares strictly between toSquare and fromSquare are empty
	bool FileOpen(char fromSquare, char toSquare) {
		int direction = (toSquare - fromSquare) > 0 ? 8 : -8;
		for (int i = fromSquare + direction; i != toSquare; i += direction) {
			if (squares[i])
				return false;
		}
		return true;
	}

	// REQUIRES: fromSquare and toSquare to be on the same rank
	// EFFECTS: returns true iff all squares strictly between toSquare and fromSquare are empty
	bool RankOpen(char fromSquare, char toSquare) {
		int direction = (toSquare - fromSquare) > 0 ? 1 : -1;
		for (int i = fromSquare + direction; i != toSquare; i += direction) {
			if (squares[i])
				return false;
		}
		return true;
	}

	// REQUIRES: fromSquare and toSquare to be on the same diagonal
	// EFFECTS: returns true iff all squares strictly between toSquare and fromSquare are empty
	bool DiagOpen(char fromSquare, char toSquare) {
		int direction = (toSquare - fromSquare) > 0 ? 8 : -8;
		direction += ((File(toSquare) - File(fromSquare)) > 0 ? 1 : -1);
		for (int i = fromSquare + direction; i != toSquare; i += direction) {
			if (squares[i])
				return false;
		}
		return true;
	}
	bool IsSquare(int square) {return (-1 < square || square > 63); }

	int TryFindAndMovePiece(char piece, int toSquare, Move move);
    bool IsLegalMove(int orginalSquare, int toSquare, Move move);

	void SetEnPassantSquareWhite(int fromSquare, int toSquare, Move move);
	void SetEnPassantSquareBlack(int fromSquare, int toSquare, Move move);

	void SetCastleRightsWhite(int fromSquare, int toSquare);
	void SetCastleRightsBlack(int fromSquare, int toSquare);

	int (*Direction) (int);

	bool IsFirstInDirection(int square, Direction, char piece0, char piece1 = NULL_PIECE) {
		for (int testSquare = Direction(square); IsSquare(testSquare) ; Direction(square)) {
			if (square[i])
				return (square[i] == piece0 || square[i] == piece1);
		}
		return false;
	}

	bool IsThreatenedByWhite(int square) { return false; }

	bool IsThreatenedByBlack(int square) { 

		int testSquare = MoveRightUpKnight(square);
		if (IsSquare(testSquare) && squares[testSquare] == knight) return false;

		testSquare = MoveUpRightKnight(square);
		if (IsSqaure(testSquare) && squares[testSquare] == knight) return false;

		testSquare = MoveRightDownKnight(square);
		if (IsSqaure(testSquare) && squares[testSquare] == knight) return false;

		testSquare = MoveLeftDownKnight(square);
		if (IsSqaure(testSquare) && squares[testSquare] == knight) return false;

		
		if IsFirstInDirection(square, &MoveUp(), ROOK_W, QUEEN_W) return false;
		if IsFirstInDirection(square, &MoveUp(), ROOK_W, QUEEN_W) return false;
		if IsFirstInDirection(square, &MoveUp(), ROOK_W, QUEEN_W) return false;
		if IsFirstInDirection(square, &MoveUp(), ROOK_W, QUEEN_W) return false;

	}

	int MoveUp(int square) { return square + 8; }
	int MoveDown(int square) { return square - 8; }
	int MoveRight(int square) { return square + 1; }
	int MoveLeft(int square) { return square - 1; }

	int MoveUpRight(int square) { return MoveUp(MoveRight(square)); }
	int MoveDownRight(int square) { return MoveDown(MoveRight(square)); }
	int MoveDownLeft(int square) { return MoveDown(MoveLeft(square)); }
	int MoveUpLeft(int square) { return MoveUp(MoveLeft(square)); }

	int MoveUpRightKnight(int square) { return MoveUp(MoveUp(MoveRight(square))); }
	int MoveDownRightKnight(int square) { return MoveDown(MoveDown(MoveRight(square))); }
	int MoveDownLeftKnight(int square) { return MoveDown(MoveDown(MoveLeft(square))); }
	int MoveUpLeftKnight(int square) { return MoveUp(MoveUp(MoveLeft(square))); }

	int MoveRightUpKnight(int square) { return MoveRight(MoveRight(MoveUp(square))); }
	int MoveRightDownKnight(int square) { return MoveRight(MoveRight(MoveDown(square))); }
	int MoveLeftDownKnight(int square) { return MoveLeft(MoveLeft(MoveDown(square))); }
	int MoveLeftUpKnight(int square) { return MoveLeft(MoveLeft(MoveUp(square))); }



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

		auto prevState = new BoardState(*_state);

		if (_state->TryMove(move)) {
			delete prevState;
			return true;
		}
		
		delete _state;
		_state = prevState;	
		
		return false;
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

