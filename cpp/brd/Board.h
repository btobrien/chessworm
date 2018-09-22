#pragma once

#include "BoardState.h"
#include <string>

class Board {
public:
	Board() {}
	Board(const Board& rhs) {}

	~Board();

	bool TryUpdate(const std::string& moveStr) override {

		try { Move move(moveStr); }
		catch { return false; }

		auto prevState = copyState();
		if (!TryUpdateState(move)) {
			delete SwapState(prevState);
			return false;
		}
		delete prevState;
		return true;
	}

    inline bool whiteToMove() const { return state->whiteToMove(); }
	inline int clock() const { return state->clock(); }
	inline bool whiteCastleShort() const { return state->whiteCastleShort(); }
	inline bool whiteCastleLong()  const { return state->whiteCastleLong(); }
	inline bool blackCastleShort() const { return state->blackCastleShort(); }
	inline bool blackCastleLong()  const { return state->blackCastleLong(); }
	inline int enPassant() const { return state->enPassant(); }
	inline std::string key() const { return state->key(); }
	inline char operator[](int i) const { return state->[i]; } 
private:
	inline bool TryUpdateState(Move move) { return whiteToMove() ? state->TryMove<White::BoardState>(move) : state->TryMove<Black::BoardState>(move); }
	BoardState* state;
};

