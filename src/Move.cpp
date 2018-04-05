
#pragma once

#include <iostream>
#include <string>



struct Move {

	static const char PAWN = 'P';
	static const char KNIGHT = 'N';
	static const char BISHOP = 'B';
	static const char ROOK = 'R';
	static const char QUEEN = 'Q';
	static const char KING = 'K';

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

		if (IsCastlesLong(move)) {
			castleLong = true;
			piece = KING;
			isValid = true;
			return;
		}	
		if (IsCastlesShort(move)) {
			castleShort = true;
			piece = KING;
			isValid = true;
			return;
		}	

		if (IsValidPiece(move[0])) {
			piece = move[0];
			frontIndex = 1;
		}
		else if (IsValidFile(move[0])) {
			fromFile = move[0];
			piece = PAWN;
		}
		else return;

		if (move[backIndex - 1] == '=') {
			if (piece != PAWN)
				return;
			if (move[backIndex] == KING || !IsValidPiece(move[backIndex]))
				return;
			promoted = move[backIndex];
			backIndex -= 2;
		}

		if (backIndex < frontIndex)
			return;

		toRank = move[backIndex--];
		if (!IsValidRank(toRank))
			return;

		if (piece == PAWN) {
			if ((promoted != 0) != (toRank == '1' || toRank == '8'))
				return;
		}

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
			case KNIGHT:
			case BISHOP:
			case ROOK:
			case QUEEN:
			case KING:
				return true;
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

	static bool IsCastlesLong(std::string move) {
		if (move.length() < 5)
			return false;
		move = move.substr(0,5);
		return (move == "o-o-o" || move == "O-O-O");
	}

	static bool IsCastlesShort(std::string move) {
		if (move.length() < 3)
			return false;
		move = move.substr(0,3);
		return (move == "o-o" || move == "O-O");
	}
};


