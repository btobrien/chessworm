
#include "chess/include/board.h"
#include "move.h"
#include "board_state.h"
#include "white.h"
#include "black.h"

Board::Board() : state(new BoardState()) {}
Board::Board(const Board& other) { state = new BoardState(*(other.state)); } 
Board::~Board() { delete state; }

bool Board::TryUpdate(const std::string& moveStr) {
	try {
		Move move(moveStr);
		auto prevState = new BoardState(*state);
		if (!TryUpdateState(move)) {
			delete state;
			state = prevState;
			prevState = nullptr;
			return false;
		}
		delete prevState;
		return true;
	}
	catch(...) { return false; }
}

bool Board::TryUpdateState(Move move) { return whiteToMove() ? state->TryMove<White>(move) : state->TryMove<Black>(move); }

int Board::clock() const { return state->clock(); }
bool Board::whiteToMove() const { return clock() % 2 == 0; }
bool Board::whiteCastleShort() const { return state->whiteCastleShort(); }
bool Board::whiteCastleLong() const { return state->whiteCastleLong(); }
bool Board::blackCastleShort() const { return state->blackCastleShort(); }
bool Board::blackCastleLong() const { return state->blackCastleLong(); }
int Board::enPassant() const { return state->enPassant(); }
char Board::operator[](int i) const { return (*state)[i]; } 

