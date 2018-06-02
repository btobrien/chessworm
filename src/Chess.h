#pragma once
#include "BoardState.h"

namespace Chess {

const char PAWN = 'P';
const char KNIGHT = 'N';
const char BISHOP = 'B';
const char ROOK = 'R';
const char QUEEN = 'Q';
const char KING = 'K';
const char nullpiece = 0;  // MUST BE 0 for nullness

bool isPiece(char p) {
	switch (p) {
		case KNIGHT:
		case BISHOP:
		case ROOK:
		case QUEEN:
		case KING:
			return true;
		default:
			return false;
	}
}

const int NUM_SQUARES = 64;
enum { a1, b1, c1, d1, e1, f1, g1, h1,
	   a2, b2, c2, d2, e2, f2, g2, h2,
	   a3, b3, c3, d3, e3, f3, g3, h3,
	   a4, b4, c4, d4, e4, f4, g4, h4,
	   a5, b5, c5, d5, e5, f5, g5, h5,
	   a6, b6, c6, d6, e6, f6, g6, h6,
	   a7, b7, c7, d7, e7, f7, g7, h7,
	   a8, b8, c8, d8, e8, f8, g8, h8, nullsquare = -1 };

inline bool isSquare(int square) { return (0 <= square && square < NUM_SQUARES); }
inline char file(int square) { return (square % 8) + 'a'; }
inline char rank(int square) { return (square / 8) + '1'; }
inline int toSquare(char file, char rank) { return (rank - '1') * 8 + file - 'a'; }

inline std::string toString(int square) {
	string result(2);
	result[0] = file(square);
	result[1] = rank(square);
	return result;
}

bool isFile(char f) {
	return (f >= 'a' && f <= 'h');
}

bool isRank(char r) {
	return (r >= '1' && r <= '8');
}

bool isCastleLong(std::string move) {
	if (move.length() < 5)
		return false;
	move = move.substr(0,5);
	return (move == "o-o-o" || move == "O-O-O");
}

bool isCastleShort(std::string move) {
	if (move.length() < 3)
		return false;
	move = move.substr(0,3);
	return (move == "o-o" || move == "O-O");
}

//TMP?
const int UP  = 8;
const int RIGHT  = 1;
const int DOWN = -8;
const int LEFT  = -1;

const int UP_UP  = 16;
const int DOWN_DOWN = -16;

const int UP_RIGHT  = UP + RIGHT;
const int DOWN_RIGHT = DOWN + RIGHT;
const int DOWN_LEFT  = DOWN + LEFT;
const int UP_LEFT  = UP + LEFT;

const int UP_UP_RIGHT = UP + UP + RIGHT;
const int UP_RIGHT_RIGHT = UP + RIGHT + RIGHT;
const int DOWN_RIGHT_RIGHT = DOWN + RIGHT + RIGHT;
const int DOWN_DOWN_RIGHT = DOWN + DOWN + RIGHT;
const int DOWN_DOWN_LEFT = DOWN + DOWN + LEFT;
const int DOWN_LEFT_LEFT = DOWN + LEFT + LEFT;
const int UP_LEFT_LEFT = UP + LEFT + LEFT;
const int UP_UP_LEFT = UP + UP + LEFT;

// factor fen functions out into different components
template <typename BoardT>
std::string fen(const BoardT& board) {
	std::string result;
    int empties = 0;

    for (int i = 0; i < NUM_SQUARES; i++) {
        if (board[i]) {
            if (empties) {
                result += std::to_string(empties);
                empties = 0;
            }
            result += board[i];
        }
        else {
            empties++;
        }
        
        if (file(i) == 'h') {
            if (empties) {
                result += std::to_string(empties);
                empties = 0;
            }
            result += '/';
        }
    }
    result.pop_back(); //delete ending slash
    
    result += ' ';
	result += board.whiteToMove() ? 'w' : 'b';
    result += ' ';
    
    if (!board.whiteCastleShort() &&
		!board.whiteCastleLong &&
		!board.blackCastleShort() &&
		!board.blackCastleLong()) { result += '-'; }
	else {
		if (board.whiteCastleShort())
			result += White::KING;
		if (board.whiteCastleLong)
			result += White::QUEEN;
		if (board.blackCastleShort())
			result += Black::KING;
		if (board.blackCastleLong())
			result += Black::QUEEN;
	}
		
    result += ' ';
    if (board.enPassant() < 0)
        result += '-';
    else {
        result += file(enPassant);
        result += rank(enPassant);
    }
	Logger::log("Chess::fen returning " + result);
    return result;
}

}



