
#pragma once

#include <iostream>
#include <string>

namespace Chess {

class Move {
public:
	inline char piece();
	inline char newFile();
	inline char newRank();
	inline char oldFile();
	inline char oldRank();

	inline bool isShortCastle() { return _castleShort; }
	inline bool isLongCastle() { return _castleLong; }

	inline bool check() { return _check; }
	inline bool checkMate() { return _checkMate; }
	inline bool takes() { return _takes; }
	inline char promotion() { return _promotion; }


	bool TryMatch(char piece, char oldFile, char oldRank) {

		if (_oldFile && oldFile != _oldFile)
			return false;
		if (_oldRank && oldRank != _oldRank)
			return false;
		if (oldFile == _newFile && oldRank ==  _newRank)
			return false;
		_oldRank = oldRank;
		_oldFile = oldFile;
		
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
			case Chess::KING:   return (manhattanDiff && manhattanDist == 1) ||
									   (!manhattanDiff && manhattanDist == 2);
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
			check = true;
			backIndex--;
		}
		else if (move[backIndex] == '#') {
			check = true;
			mate = true;
			backIndex--;
		}

		if (IsCastlesLong(move)) {
			castleLong = true;
			piece = KING;
			return;
		}	
		if (IsCastlesShort(move)) {
			castleShort = true;
			piece = KING;
			return;
		}	

		if (Piece(move[0])) {
			piece = move[0];
			frontIndex = 1;
		}
		else if (isFile(move[0])) {
			oldFile = move[0];
			piece = PAWN;
		}
		else throw Exception;

		if (move[backIndex - 1] == '=') {
			if (piece != PAWN)
				throw Exception;
			if (move[backIndex] == KING || !isPiece(move[backIndex]))
				throw Exception;
			promoted = move[backIndex];
			backIndex -= 2;
		}

		if (backIndex < frontIndex)
			throw Exception;

		newRank = move[backIndex--];
		if (!isRank(newRank))
			throw Exception;

		if (piece == PAWN) {
			if ((promoted != 0) != (newRank == '1' || newRank == '8'))
				throw Exception;
		}

		if (backIndex < frontIndex)
			throw Exception;

		newFile = move[backIndex--];
		if (!isFile(newFile))
			throw Exception;

		if (backIndex < frontIndex)
			return;

		if (move[backIndex] == 'x') {
			takes = true;
			backIndex--;
		}

		if (isRank(move[backIndex]))
			oldRank = move[backIndex--];
					
		if (backIndex < frontIndex)
			return;

		if (isFile(move[backIndex]))
			oldFile = move[backIndex--];

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

oldSquare(const Move& move) { 
	if (!move.oldFile() || !move.oldRank())
		return false;
	return toSquare(move.oldFile(), move.oldRank());
}

newSquare(const Move& move) { return toSquare(move.newFile(), move.newRank()); }

}
