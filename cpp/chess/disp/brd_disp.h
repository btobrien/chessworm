#pragma once

#include <iostream>

const int BOARD_WIDTH = 8;

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

int fullClock(int clock) {
	return (clock + 1) / 2;
}

std::string prefix(int clock) { 
	std::string dot = (clock % 2 == 0) ? ". " : "...";
	return std::to_string(fullClock(clock)) + dot; 
}
