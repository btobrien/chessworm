#pragma once

#include "squares.h"
#include "pieces.h"
#include "castle_flags.h"
#include "include/read.h"
#include <string>
#include <sstream>
#include <cctype>

namespace fen {

bool whiteToMove(int clock) {
	return clock % 2 == 0;
}

//use string stream;
template<typename T>
std::string to_string(const T& squares) {
	std::stringstream fen;
    int empties = 0;

	for (int row = BOARD_WIDTH - 1; row >= 0; row--) {
		for (int col = 0; col < BOARD_WIDTH; col++) {
			char piece = squares[row * BOARD_WIDTH + col];
			if (piece) {
				if (empties) {
					fen << std::to_string(empties);
					empties = 0;
				}
				fen << piece;
			}
			else
				empties++;
		}
		if (empties) {
			fen << std::to_string(empties);
		}
		fen << '/';
		empties = 0;
	}
	std::string result = fen.str();
	result.pop_back(); // remove last slash
	return result;
}

std::string to_string(const CastleFlags flags) {
	std::stringstream fen;
    if (!flags.whiteCastleShort &&
		!flags.whiteCastleLong &&
		!flags.blackCastleShort &&
		!flags.blackCastleLong)
	{
		return "-";
	}
	else {
		if (flags.whiteCastleShort)
			fen << 'K';
		if (flags.whiteCastleLong)
			fen << 'Q';
		if (flags.blackCastleShort)
			fen << 'k';
		if (flags.blackCastleLong)
			fen << 'q';
		return fen.str();
	}
}

template<typename T>
std::string to_string(const T& squares, const CastleFlags flags, int clock, int en_passant = -1) {
	std::stringstream fen;
	fen << to_string(squares);
    fen << ' ';
	fen << (whiteToMove(clock) ? 'w' : 'b');
    fen << ' ';
	fen << to_string(flags);
    fen << ' ';
	fen << squares::to_string(en_passant);
    return fen.str();
}

#include <iostream>

std::string board(const std::string& fen) {
	std::stringstream brd;
	int i = 0;
	for (int row = 0; row < BOARD_WIDTH; row++) {
		char c;
		c = fen[i++];
		while(c != '/' && c != ' ') {
			if (!isdigit(c)) {
				brd << c;
			}
			else {
				int empties = c - '0';
				while (empties-- > 0 )
					brd << ' ';
			}
			c = fen[i++];
		}
	}
	return brd.str();
}

int full_clock(int clock) {
	return (clock + 1) / 2;
}

std::string prefix(int clock) { 
	std::string dot = whiteToMove(clock) ? ". " : "...";
	return std::to_string(full_clock(clock)) + dot; 
}

}
