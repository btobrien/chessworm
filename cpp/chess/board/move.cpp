
#include <ctgmath>
#include "move.h"
using namespace Chess;

char Move::newSquare() const { return toSquare(_newFile, _newRank); }

bool Move::tryMatch(char piece, int oldSquare) const {
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
		case PAWN:
			if (!rankDist)
				return false;
			if (!manhattanDiff)
				return manhattanDist == 2;
			if (fileDist)
				return false;
			if (rankDist > 2)
				return false;
			return oldRank == '2' || oldRank == '7' || rankDist == 1;
		case KNIGHT: return manhattanDist == 3 && manhattanDiff == 1;
		case BISHOP: return !manhattanDiff;
		case ROOK:   return !rankDist || !fileDist;
		case QUEEN:  return !manhattanDiff || !rankDist || !fileDist;
		case KING:   return (manhattanDiff && manhattanDist == 1) || (!manhattanDiff && manhattanDist == 2);
	}
	return false;
}


// TODO: break this up into more functions?
Move::Move(std::string move) {
	int frontIndex = 0;
	int backIndex = move.length() - 1; 

	if (backIndex <= frontIndex)
		throw;

	if (move[backIndex] == '#') {
		_check = true;
		_mate = true;
		backIndex--;
	}
	while (move[backIndex] == '+') { // support double check notation
		_check = true;
		backIndex--;
	}

	if (isCastleLong(move)) {
		_castleLong = true;
		_piece = KING;
		return;
	}	
	if (isCastleShort(move)) {
		_castleShort = true;
		_piece = KING;
		return;
	}	

	if (isPiece(move[0])) {
		_piece = move[0];
		frontIndex = 1;
	}
	else if (isFile(move[0])) {
		_oldFile = move[0];
		_piece = PAWN;
	}
	else throw;

	if (move[backIndex - 1] == '=') {
		if (_piece != PAWN)
			throw;
		if (move[backIndex] == KING || !isPiece(move[backIndex]))
			throw;
		_promotion = move[backIndex];
		backIndex -= 2;
	}

	if (backIndex < frontIndex)
		throw;

	_newRank = move[backIndex--];
	if (!isRank(_newRank))
		throw;

	if (_piece == PAWN) {
		if ((_promotion != 0) != (_newRank == '1' || _newRank == '8'))
			throw;
	}

	if (backIndex < frontIndex)
		throw;

	_newFile = move[backIndex--];
	if (!isFile(_newFile))
		throw;

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
		throw;
}
