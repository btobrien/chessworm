#pragma once

#include "squares.h"
#include <string>

namespace fen {

bool whiteToMove(int clock) {
	return clock % 2 == 0;
}

template<typename T>
std::string to_string(const T& squares) {
	std::string fen;
    int empties = 0;

	for (int row = BOARD_WIDTH - 1; row >= 0; row--) {
		for (int col = 0; col < BOARD_WIDTH; col++) {
			char piece = squares[row * BOARD_WIDTH + col];
			if (piece) {
				if (empties) {
					fen += std::to_string(empties);
					empties = 0;
				}
				fen += piece;
			}
			else
				empties++;
		}
		if (empties) {
			fen += std::to_string(empties);
		}
		fen += '/';
		empties = 0;
	}
    fen.pop_back(); //removes last slash
	return fen;
}

std::string to_string(const CastleFlags flags) {
	std::string fen;
    if (!flags.whiteCastleShort &&
		!flags.whiteCastleLong &&
		!flags.blackCastleShort &&
		!flags.blackCastleLong)
	{
		return "-";
	}
	else {
		if (flags.whiteCastleShort)
			fen += 'K';
		if (flags.whiteCastleLong)
			fen += 'Q';
		if (flags.blackCastleShort)
			fen += 'k';
		if (flags.blackCastleLong)
			fen += 'q';
		return fen;
	}
}

template<typename T>
std::string to_string(const T& squares, const CastleFlags flags, int clock, int en_passant = -1) {
	std::string fen;
	fen += to_string(squares);
    fen += ' ';
	fen += whiteToMove(clock) ? 'w' : 'b';
    fen += ' ';
	fen += to_string(flags);
    fen += ' ';
	fen += squares::to_string(en_passant);
    return fen;
}

int full_clock(int clock) {
	return (clock + 1) / 2;
}

std::string prefix(int clock) { 
	std::string dot = whiteToMove(clock) ? ". " : "...";
	return std::to_string(full_clock(clock)) + dot; 
}

}
