#pragma once

#include <string>

class Move;
class BoardState;

class Board {
public:
	Board();
	Board(const Board&);
	~Board();

	bool TryUpdate(const std::string& moveStr);

    bool whiteToMove() const;
	int clock() const;
	bool whiteCastleShort() const;
	bool whiteCastleLong()  const;
	bool blackCastleShort() const;
	bool blackCastleLong()  const;
	int enPassant() const;
	std::string key() const;
	char operator[](int i) const;
private:
	bool TryUpdateState(Move move);
	BoardState* state;
};

