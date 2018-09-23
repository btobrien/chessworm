#pragma once

#include <string>

namespace Chess {

const char PAWN = 'P';
const char KNIGHT = 'N';
const char BISHOP = 'B';
const char ROOK = 'R';
const char QUEEN = 'Q';
const char KING = 'K';
const char nullpiece = 0;  // MUST BE 0 for nullness
bool isPiece(char p);
bool isCastleLong(std::string move);
bool isCastleShort(std::string move);
}
