
#pragma once

#include <iostream>
#include <string>

namespace Chess {

class Move {
public:
	inline char newSquare() const  { return toSquare(_newFile, _newRank); }

	inline bool isShortCastle() const { return _castleShort; }
	inline bool isLongCastle() const { return _castleLong; }

	inline bool check() const { return _check; }
	inline bool checkMate() const { return _checkMate; }
	inline bool takes() const { return _takes; }

	inline char promotion() const { return _promotion; }


	bool tryMatch(char piece, int oldSquare) {
		if (_piece && piece != _piece)
			return false;
		int oldFile = file(oldSquare)
		int oldRank = rank(oldSquare)
		if (_oldFile && oldFile != _oldFile)
			return false;
		if (_oldRank && oldRank != _oldRank)
			return false;
		if (oldFile == _newFile && oldRank ==  _newRank)
			return false;
		
		int fileDist = abs(_oldFile - newFile);
		int rankDist = abs(oldRank - newRank);
		int manhattanDist = fileDistance() + rankDistance();
		int manhattanDiff = abs(fileDistance - rankDistance);

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


	// break this up into more functions
	Move(std::string move) {
		int frontIndex = 0;
		int backIndex = move.length() - 1; 

		if (backIndex <= frontIndex)
			throw Exception;

		while (move[backIndex] == '+') { // support double check notation
			_check = true;
			_backIndex--;
		}
		else if (move[backIndex] == '#') {
			_check = true;
			_mate = true;
			backIndex--;
		}

		if (isCastlesLong(move)) {
			_castleLong = true;
			_piece = KING;
			return;
		}	
		if (isCastlesShort(move)) {
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
		else throw Exception;

		if (move[backIndex - 1] == '=') {
			if (_piece != PAWN)
				throw Exception;
			if (move[backIndex] == KING || !isPiece(move[backIndex]))
				throw Exception;
			_promoted = move[backIndex];
			backIndex -= 2;
		}

		if (backIndex < frontIndex)
			throw Exception;

		newRank = move[backIndex--];
		if (!isRank(newRank))
			throw Exception;

		if (_piece == PAWN) {
			if ((_promoted != 0) != (_newRank == '1' || _newRank == '8'))
				throw Exception;
		}

		if (backIndex < frontIndex)
			throw Exception;

		_newFile = move[backIndex--];
		if (!isFile(_newFile))
			throw Exception;

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
			throw Exception;
	}

private:
	char _piece;
	bool _castleLong;
	bool _castleShort;
	char _oldFile;
	char _oldRank;
	bool _takes;
	char _newFile;
	char _newRank;
	char _promoted;
	bool _check;
	bool _mate;
};

}
