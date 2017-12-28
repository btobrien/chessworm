#include <iostream>
#include <string>

struct Move {

	char piece;
	bool castleLong;
	bool castleShort;
	char fromFile;
	char fromRank;
	bool takes;
	char toFile;
	char toRank;
	char promoted;
	bool check;
	bool mate;
	bool isValid;
	
	Move(const std::string& move) : piece(0), 
									castleLong(false),
									castleShort(false),
									fromFile(0),
									fromRank(0),
									takes(false),
									toFile(0),
									toRank(0),
									promoted(0),
									check(false),
									mate(false),
									isValid(false) {


		int frontIndex = 0;
		int backIndex = move.length() - 1; 

		if (backIndex <= frontIndex)
			return;

		if (move[backIndex] == '+') {
			check = true;
			backIndex--;
		}
		else if (move[backIndex] == '#') {
			check = true;
			mate = true;
			backIndex--;
		}

		if (move.length() >= 5 && move.substr(0,5) == "o-o-o") {
			castleLong = true;
			piece = 'K';
			isValid = true;
			return;
		}	
		if (move.length() >= 3 && move.substr(0,3) == "o-o") {
			castleShort = true;
			piece = 'K';
			isValid = true;
			return;
		}	

		if (IsValidPiece(move[0])) {
			piece = move[0];
			frontIndex = 1;
		}
		else if (IsValidFile(move[0])) {
			fromFile = move[0];
			piece = 'P';
		}
		else
			return;


		if (move[backIndex - 1] == '=') {
			if (piece != 'P')
				return;
			if (move[backIndex] == 'K' || !IsValidPiece(move[backIndex]))
				return;
			promoted = move[backIndex];
			backIndex -= 2;
		}

		if (backIndex < frontIndex)
			return;

		toRank = move[backIndex--];
		if (!IsValidRank(toRank))
			return;

		if (promoted != (toRank == '1' || toRank == '8'))
			return;

		if (backIndex < frontIndex)
			return;

		toFile = move[backIndex--];
		if (!IsValidFile(toFile))
			return;


		isValid = true;
	
		if (backIndex < frontIndex)
			return;


		if (move[backIndex] == 'x') {
			takes = true;
			backIndex--;
		}

		if (IsValidRank(move[backIndex]))
			fromRank = move[backIndex--];
					
		if (backIndex < frontIndex)
			return;

		if (IsValidFile(move[backIndex]))
			fromFile = move[backIndex--];

		isValid = backIndex < frontIndex;
	}
	
	static bool IsValidPiece(char p) {
		switch (p) {
			case 'N':
			case 'B':
			case 'R':
			case 'Q':
			case 'K':
				return true;
				break;

			default:
				return false;
		}
	}

	static bool IsValidFile(char f) {
		return (f >= 'a' && f <= 'h');
	}

	static bool IsValidRank(char r) {
		return (r >= '1' && r <= '8');
	}

};


