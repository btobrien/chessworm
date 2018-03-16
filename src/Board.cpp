
#include <stdio.h>
#include "BoardState.h"
#include "Color.h"
#include <iostream>
#include <string>
#include <stack>
#include <vector>
#include <cassert>
#include "Utils.cpp"
//#include "Parse.h" for GLYPH parsing

using std::stack;
using std::vector;
using std::string;

class Board {

public:
	Board() : _state(new BoardState()) {}
	Board(const Board& rhs) : _state(new BoardState(*rhs._state)) {}
	Board(BoardState* state) : _state(state) {}

	virtual bool TryMove(const std::string& m) {
	
		Move move(m);
		if (!move.isValid) {
			return false;
		}

		auto prevState = new BoardState(*_state);

		if (TryUpdateState(move)) {
			delete prevState;
			return true;
		}
		
		delete _state;
		_state = prevState;	
		
		return false;
	}

    inline bool whiteToMove() const { return _state->clock % 2 == 0; }
	int clock() const { return _state->clock; }
	std::string key() const { return _state->ToString(); }
	const char* data() const { return _state->squares; }
	char operator [](int i) const { return _state->squares[i]; }


protected:
	BoardState* _state;

	inline bool TryUpdateState(Move move) { return whiteToMove() ? _state->TryMove<White>(move) : _state->TryMove<Black>(move); }
};


class MemoryBoard : public Board {
	
public:

    virtual bool TryMove(const std::string& m) override {


		Move move(m);

		if (!move.isValid)
			return false;

		_history.push(_state);
		_state = new BoardState(*_state);

		if (!TryUpdateState(move)) {
			delete _state;
			_state = _history.top();
			_history.pop();
			return false;
		}	
		
		while (!_future.empty()) {
			delete _future.top();
			_future.pop();
			delete _moveList.back();
			_moveList.pop_back();
		}

		_moveList.push_back(new string(m));

		return true;
	}			

    virtual bool TryUndo() { 
		if (_history.empty())
			return false;
		_future.push(_state);
		_state = _history.top();
		_history.pop();
		return true;
	}

    virtual bool TryRedo() { 
		if (_future.empty())
			return false;
		_history.push(_state);
		_state = _future.top();
		_future.pop();
		return true;
	}

	template <typename Container>
	void GetMoveHistory(Container& container) {
		container.clear();
		push_all(_moveList, container);
	}

	string prev_key() {
		if (!TryUndo())
			return "";
		string result = key();
		TryRedo();
		return result;
	}
	
protected:
	stack<BoardState*> _history;
	stack<BoardState*> _future;
	vector<const string*> _moveList;
};

