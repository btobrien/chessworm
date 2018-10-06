#pragma once
#include <iostream>

const int BOARD_WIDTH = 8;

void DisplayPiece(char piece);

inline void DisplaySquare(char piece) {
	std::cout << ' ';
	DisplayPiece(piece);
}

inline bool isRowDelim(char c) {
	return (c == '/' || c == ' ' || c == '_');
}

void DisplayFen(const std::string& fen) {
	int i = 0;
	for (int row = 0; row < BOARD_WIDTH; row++) {
		char c;
		c = fen[i++];
		while(!isRowDelim(c)) {
			if (!isdigit(c))
				DisplaySquare(c);
			else {
				int empties = c - '0';
				while (empties-- > 0 )
					DisplaySquare(' ');
			}
			c = fen[i++];
		}
		std::cout << '\n';
	}
}
