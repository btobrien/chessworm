#pragma once

#include "chess/include/castle_flags.h"
#include <string>

class Move;
class BoardState;

class Board {
public:
	Board();
	Board(const Board&);
	~Board();

	bool TryMove(const std::string&);

	char operator[](int i) const;
	int clock() const;
	CastleFlags flags() const;
	int enPassant() const;
private:
	bool TryUpdateState(Move);
	BoardState* state;
};

