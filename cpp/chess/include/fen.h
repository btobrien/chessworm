#pragma once

#include "squares.h"
#include "pieces.h"
#include "board.h"

struct board_state
std::string fen(const board_state& brd) {
	std::string fen;
    int empties = 0;

    for (int i = 0; i < NUM_SQUARES; i++) {
        if (brd[i]) {
            if (empties) {
                fen += std::to_string(empties);
                empties = 0;
            }
            fen += brd[i];
        }
        else {
            empties++;
        }
        
        if (file(i) == 'h') {
            if (empties) {
                fen += std::to_string(empties);
                empties = 0;
            }
            fen += '/';
        }
    }
    fen.pop_back(); //delete ending slash
    
    fen += ' ';
	fen += brd.whiteToMove() ? 'w' : 'b';
    fen += ' ';
    
    if (!brd.whiteCastleShort() &&
		!brd.whiteCastleLong &&
		!brd.blackCastleShort() &&
		!brd.blackCastleLong()) { fen += '-'; }
	else {
		if (brd.whiteCastleShort())
			fen += White::KING;
		if (brd.whiteCastleLong)
			fen += White::QUEEN;
		if (brd.blackCastleShort())
			fen += Black::KING;
		if (brd.blackCastleLong())
			fen += Black::QUEEN;
	}
		
    fen += ' ';
    if (brd.enPassant() < 0)
        fen += '-';
    else {
        fen += file(brd.enPassant);
        fen += rank(brd.enPassant);
    }
    return fen;
}

board_state init(std::string fen);
