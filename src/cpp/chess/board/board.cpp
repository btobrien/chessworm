
#include "chess/include/board.h"
#include "move.h"
#include "board_state.h"
#include "white.h"
#include "black.h"
#include <cstring>
#include <iostream>
using std::cerr;
using std::endl;

Board::Board() : state(new BoardState()) {}
Board::Board(const Board& other) : state(new BoardState(*(other.state))) {}
Board::Board(const std::string& fen) {
	state = fen.empty() ?
		new BoardState() :
		new BoardState(fen::board(fen), fen::flags(fen), fen::blackToMove(fen), fen::en_passant(fen)); 
}

Board::~Board() { delete state; }

bool Board::TryMove(const std::string& moveStr) {
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
	} catch(...) {
		std::cerr << "ERROR: move=" << moveStr << " invalid" << std::endl;
		return false;
	}
}

bool Board::TryUpdateState(Move move) { return (clock() % 2) == 0 ? state->TryMove<White>(move) : state->TryMove<Black>(move); }

int Board::clock() const { return state->clock(); }
CastleFlags Board::flags() const { return state->flags(); }
int Board::enPassant() const { return state->enPassant(); }
char Board::operator[](int i) const { return (*state)[i]; } 

