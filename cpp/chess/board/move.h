
#pragma once

#include <string>

class Move {
public:
	Move(std::string);
	bool tryMatch(char piece, int oldSquare) const;
	inline int newSquare() const { return toSquare(_newFile, _newRank); }
	inline bool castleShort() const { return _castleShort; }
	inline bool castleLong() const { return _castleLong; }
	inline bool check() const { return _check; }
	inline bool mate() const { return _mate; }
	inline bool takes() const { return _takes; }
	inline char promotion() const { return _promotion; }
private:
	char _piece;
	bool _castleLong;
	bool _castleShort;
	char _oldFile;
	char _oldRank;
	bool _takes;
	char _newFile;
	char _newRank;
	char _promotion;
	bool _check;
	bool _mate;

	bool isBigPiece(char);
};

#include "move.cpp"
