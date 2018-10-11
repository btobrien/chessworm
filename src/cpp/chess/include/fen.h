#pragma once

#include "squares.h"
#include "pieces.h"
#include "castle_flags.h"
#include <string>
#include <sstream>
#include <cctype>
#include <algorithm>
#include <iostream>

namespace fen {

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

inline std::string to_string(const CastleFlags flags) {
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
	fen << '0'; //ignore half-move clock
    fen << ' ';
	fen << (clock % 2 == 0 ? 'w' : 'b');
    fen << ' ';
	fen << to_string(flags);
    fen << ' ';
	fen << squares::to_string(en_passant);
    fen << ' ';
	fen << (clock / 2) + 1;
    return fen.str();
}

inline std::string board(const std::string& fen) {
	std::stringstream rows[8];
	int i = 0;
	for (int row = 0; row < BOARD_WIDTH; row++) {
		char c;
		c = fen[i++];
		while(c != '/' && c != ' ') {
			if (!isdigit(c)) {
				rows[row] << c;
			}
			else {
				int empties = c - '0';
				while (empties-- > 0 )
					rows[row] << ' ';
			}
			c = fen[i++];
		}
	}
	std::stringstream brd;
	for (int row = BOARD_WIDTH - 1; row >= 0; row--) {
		brd << rows[row].str();
	}
	std::string result = brd.str();
	std::replace(result.begin(), result.end(), ' ', Chess::nullpiece);
	return result;
}

inline bool blackToMove(const std::string& fen) {
	std::stringstream ss(fen);
	std::string trash;
	char flag;
	ss >> trash >> flag;
	return flag == 'b';
}

inline CastleFlags flags(const std::string& fen) {
	std::stringstream ss(fen);
	std::string flag_str;
	ss >> flag_str >> flag_str >> flag_str;
	CastleFlags flags;

	if (flag_str.find('K') == std::string::npos)
		flags.whiteCastleShort = false;
	if (flag_str.find('Q') == std::string::npos)
		flags.whiteCastleLong = false;
	if (flag_str.find('k') == std::string::npos)
		flags.blackCastleShort = false;
	if (flag_str.find('q') == std::string::npos)
		flags.blackCastleLong = false;
			
	return flags;
}

inline int en_passant(const std::string& fen) {
	std::stringstream ss(fen);
	std::string square;
	ss >> square >> square >> square >> square;
	return square != "-" ? toSquare(square) : -1;
}

}
