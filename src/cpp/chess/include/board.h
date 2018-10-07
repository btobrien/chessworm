#pragma once

#include "castle_flags.h"
#include "fen.h"
#include <string>

class Move;
class BoardState;


class Board {
public:
	Board();
	Board(const Board&);
	Board(const std::string& fen);
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

namespace fen {
	inline std::string to_string(Board brd) {
		return to_string(brd, brd.flags(), brd.clock(), brd.enPassant());
	}
}
