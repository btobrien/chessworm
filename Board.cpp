
#include <stdio.h>
#include "BoardState.h"
#include "Color.h"
#include <iostream>
#include <string>
#include <stack>
#include <cassert>


class Board {

public:
	Board() : _clock(0), _state(new BoardState()) {}
	Board(const Board& rhs) : _state(new BoardState(*rhs._state)) {}

	virtual bool TryMove(const std::string& m) {
	
		Move move(m);

		if (!move.isValid) {
			std::cerr << m << ": Invalid Move\n";
			return false;
		}
		std::cerr << m << ": Valid Move\n";

		auto prevState = new BoardState(*_state);

		if (TryUpdateBoard(move)) {
			delete prevState;
			_clock++;
			return true;
		}
		
		delete _state;
		_state = prevState;	
		
		return false;
	}

    inline bool WhiteToMove() const { return _clock % 2 == 0; }
	int clock() const { return _clock; }
	std::string ToString() const { return _state->ToString(WhiteToMove()); }
	const char* data() const { return _state->squares; }
	char operator [](int i) const { return _state->squares[i]; }


protected:
    int  _clock;
	BoardState* _state;

	inline bool TryUpdateBoard(Move move) { return WhiteToMove() ? _state->TryMove<White>(move) : _state->TryMove<Black>(move); }
};


class MemoryBoard : public Board {
	
public:

    virtual bool TryMove(const std::string& m) override {

		Move move(m);

		if (!move.isValid)
			return false;

		_history.push(_state);
		_state = new BoardState(*_state);

		if (!TryUpdateBoard(move)) {
			delete _state;
			_state = _history.top();
			_history.pop();
			return false;
		}	
		
		while (!_future.empty()) {
			delete _future.top();
			_future.pop();
		}

		_clock++;
		return true;
	}			


    bool TryUndoMove() { 
		if (_history.empty())
			return false;
		_future.push(_state);
		_state = _history.top();
		_history.pop();
		_clock--;
		return true;
	}

    bool TryRedoMove() { 
		if (_future.empty())
			return false;
		_history.push(_state);
		_state = _future.top();
		_future.pop();
		_clock++;
		return true;
	}


    
protected:
	std::stack<BoardState*> _history;
	std::stack<BoardState*> _future;
};

