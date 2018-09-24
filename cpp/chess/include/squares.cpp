
#include "squares.h"

bool isSquare(int square) { return (0 <= square && square < NUM_SQUARES); }
char file(int square) { return (square % 8) + 'a'; }
char rank(int square) { return (square / 8) + '1'; }
int toSquare(char file, char rank) { return (rank - '1') * 8 + file - 'a'; }

std::string squares::to_string(int square) {
	if (!isSquare(square))
		return "-";
	std::string result("--");
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

int lineDirection(int old_square, int new_square) { 
	int dir = 0;
	if (file(old_square) == file(new_square))
		dir = UP;
	else if (rank(old_square) == rank(new_square))
		dir = RIGHT;
	else
		return 0;
	return new_square > old_square ? dir : -1 * dir;
}
