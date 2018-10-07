
#include "squares.h"

inline bool isSquare(int square) { return (0 <= square && square < NUM_SQUARES); }
inline char file(int square) { return (square % 8) + 'a'; }
inline char rank(int square) { return (square / 8) + '1'; }
inline int toSquare(char file, char rank) { return (rank - '1') * 8 + file - 'a'; }
inline int toSquare(std::string str) { return toSquare(str[0], str[1]); }

inline std::string squares::to_string(int square) {
	if (!isSquare(square))
		return "-";
	std::string result("--");
	result[0] = file(square);
	result[1] = rank(square);
	return result;
}

inline bool isFile(char f) {
	return (f >= 'a' && f <= 'h');
}

inline bool isRank(char r) {
	return (r >= '1' && r <= '8');
}

inline int lineDirection(int old_square, int new_square) { 
	int dir = 0;
	if (file(old_square) == file(new_square))
		dir = UP;
	else if (rank(old_square) == rank(new_square))
		dir = RIGHT;
	else
		return 0;
	return new_square > old_square ? dir : -1 * dir;
}
