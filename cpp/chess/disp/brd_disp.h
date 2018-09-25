#pragma once

#include "chess/include/squares.h"
#include <iostream>


void DisplaySquare(char piece, bool isLightSquare = false);

void DisplayFen(const std::string& fen) {
	int i = 0;
	bool isLight = true;
	for (int row = 0; row < BOARD_WIDTH; row++) {
		char c;
		c = fen[i++];
		while(c != '/' && c != ' ') {
			if (!isdigit(c)) {
				DisplaySquare(c);
				isLight = !isLight;
			}
			else {
				int empties = c - '0';
				while (empties-- > 0 ) {
					DisplaySquare(' ');
					isLight = !isLight;
				}
			}
			c = fen[i++];
		}
		std::cout << '\n';
	}
}

void DiplayFenFlipped(const std::string& fen);  //TODO
