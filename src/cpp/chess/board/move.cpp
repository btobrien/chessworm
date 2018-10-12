
#include "move.h"
#include "chess/include/squares.h"
#include "chess/include/pieces.h"
#include <ctgmath>
#include <stdexcept>
#include <cstring>

inline bool Move::tryMatch(char piece, int oldSquare) const {
	if (_piece && piece != _piece)
		return false;
	int oldFile = file(oldSquare);
	int oldRank = rank(oldSquare);
	if (_oldFile && oldFile != _oldFile)
		return false;
	if (_oldRank && oldRank != _oldRank)
		return false;
	if (oldFile == _newFile && oldRank ==  _newRank)
		return false;
	
	int fileDist = std::abs(oldFile - _newFile);
	int rankDist = std::abs(oldRank - _newRank);
	int manhattanDist = fileDist + rankDist;
	int manhattanDiff = std::abs(fileDist - rankDist);

	switch (piece) {
		case Chess::PAWN:
			if (!rankDist)
				return false;
			if (!manhattanDiff)
				return manhattanDist == 2;
			if (fileDist)
				return false;
			if (rankDist > 2)
				return false;
			return oldRank == '2' || oldRank == '7' || rankDist == 1;
		case Chess::KNIGHT: return manhattanDist == 3 && manhattanDiff == 1;
		case Chess::BISHOP: return !manhattanDiff;
		case Chess::ROOK:   return !rankDist || !fileDist;
		case Chess::QUEEN:  return !manhattanDiff || !rankDist || !fileDist;
		case Chess::KING:   return (manhattanDiff && manhattanDist == 1) || (!manhattanDiff && manhattanDist == 2);
	}
	return false;
}


// TODO: break this up into more functions?
inline Move::Move(std::string move) {

	memset(this, 0, sizeof(Move));

	int frontIndex = 0;
	int backIndex = move.length() - 1; 

	if (backIndex <= frontIndex)
		throw std::invalid_argument("invalid");

	if (move[backIndex] == '#') {
		_check = true;
		_mate = true;
		backIndex--;
	}
	while (move[backIndex] == '+') { // support double check notation
		_check = true;
		backIndex--;
	}

	if (Chess::isCastleLong(move)) {
		_castleLong = true;
		_piece = Chess::KING;
		return;
	}	
	if (Chess::isCastleShort(move)) {
		_castleShort = true;
		_piece = Chess::KING;
		return;
	}	

	if (isBigPiece(move[0])) {
		_piece = move[0];
		frontIndex = 1;
	}
	else if (isFile(move[0])) {
		_oldFile = move[0];
		_piece = Chess::PAWN;
	}
	else throw std::invalid_argument("invalid");

	if (move[backIndex - 1] == '=') {
		if (_piece != Chess::PAWN)
			throw std::invalid_argument("invalid");
		if (move[backIndex] == Chess::KING || !isBigPiece(move[backIndex]))
			throw std::invalid_argument("invalid");
		_promotion = move[backIndex];
		backIndex -= 2;
	}

	if (backIndex < frontIndex)
		throw std::invalid_argument("invalid");

	_newRank = move[backIndex--];
	if (!isRank(_newRank))
		throw std::invalid_argument("invalid");

	if (_piece == Chess::PAWN) {
		if ((_promotion != 0) != (_newRank == '1' || _newRank == '8'))
			throw std::invalid_argument("invalid");
	}

	if (backIndex < frontIndex)
		throw std::invalid_argument("invalid");

	_newFile = move[backIndex--];
	if (!isFile(_newFile))
		throw std::invalid_argument("invalid");

	if (backIndex < frontIndex)
		return;

	if (move[backIndex] == 'x') {
		_takes = true;
		backIndex--;
	}

	if (isRank(move[backIndex]))
		_oldRank = move[backIndex--];
				
	if (backIndex < frontIndex)
		return;

	if (isFile(move[backIndex]))
		_oldFile = move[backIndex--];

	if (backIndex >= frontIndex)
		throw std::invalid_argument("invalid");
}

inline bool Move::isBigPiece(char p) { return p != Chess::PAWN && Chess::isPiece(p); }
