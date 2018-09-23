
#include "squares.h"

bool isSquare(int square) { return (0 <= square && square < NUM_SQUARES); }
char file(int square) { return (square % 8) + 'a'; }
char rank(int square) { return (square / 8) + '1'; }
int toSquare(char file, char rank) { return (rank - '1') * 8 + file - 'a'; }

inline std::string toString(int square) {
	char result[2];
	result[0] = file(square);
	result[1] = rank(square);
	return result;
}

bool isFile(char f) {
	return (f >= 'a' && f <= 'h');
}

bool isRank(char r) {
	return (r >= '1' && r <= '8');
}

//TODO
int lineDirection(int oldSquare, int newSquare) { return 1; }
