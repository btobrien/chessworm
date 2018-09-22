#pragma once

#include <stdio.h>
#include "BoardState.h"
#include "Color.h"
#include <iostream>
#include <string>
#include <stack>
#include <vector>
#include "Utils.cpp"
#include "Log.h"

template <typename T>
class Memorable {
protected:
	virtual bool TryUpdate(const std::string&) = 0;
	Memorable() : _state(new T()) {}
	Memorable(const Memorable& rhs) : _state(new T(*(rhs._state))) {}
	Memorable(T* state) : _state(state) {}
	virtual ~Memorable() { delete _state; }
	inline T* getState() const { return _state }
	inline T* copyState() const { return new T(*_state); }
	inline T* SwapState(T* newState) { 
		oldState = _state;
		_state = newState;
		return oldState;
	}
private:
	T* _state;
};

class Board : protected Memorable<BoardState> {
public:
	Board() {}
	Board(const Board& rhs) : Memorable(rhs) {}
	virtual ~Board();

	virtual bool TryUpdate(const std::string& moveStr) override {

		try { Move move(moveStr); }
		catch { return false; }

		auto prevState = copyState();
		if (!TryUpdateState(move)) {
			delete SwapState(prevState);
			return false;
		}
		delete prevState;
		return true;
	}

    inline bool whiteToMove() const { return getState()->whiteToMove(); }
	inline int clock() const { return getState()->clock(); }
	inline bool whiteCastleShort() const { return getState()->whiteCastleShort(); }
	inline bool whiteCastleLong()  const { return getState()->whiteCastleLong(); }
	inline bool blackCastleShort() const { return getState()->blackCastleShort(); }
	inline bool blackCastleLong()  const { return getState()->blackCastleLong(); }
	inline int enPassant() const { return getState()->enPassant(); }
	inline std::string key() const { return getState()->key(); }
	inline char operator[](int i) const { return getState()->[i]; } 
private:
	inline bool TryUpdateState(Move move) { return whiteToMove() ? getState()->TryMove<White::BoardState>(move) : getState()->TryMove<Black::BoardState>(move); }
};

template <typename T>
class Memory : public T {
public:
	virtual ~Memory() {
		while (!_future.empty()) {
			delete _future.top();
			_future.pop();
		}
		while (!_history.empty()) {
			delete _history.top();
			_fhistory.pop();
		}
	}

	virtual bool TryUpdate(const std::string& transition) override {
		_history.push(copyState());
		if (!T::TryUpdate(transition)) {
			delete _history.top();
			_history.pop();
			return false;
		}	
		_transitionList.push_back(new string(transition));
		while (!_future.empty()) {
			delete _future.top();
			_future.pop();
			_transitionList.pop_back();
		}
		return true;
	}			

	virtual bool TryUndo() { 
		if (_history.empty())
			return false;
		_future.push(SwapState(_history.top()));
		_history.pop();
		return true;
	}

	bool TryRedo() { 
		if (_future.empty())
			return false;
		_history.push(SwapState(_future.top()));
		_future.pop();
		return true;
	}

	typedef boost::ptr_vector<std::string>::iterator iterator 
	iterator begin() const; 
	iterator end() const;

private:
	typedef decltype(T::getState()) state
	std::stack<state*> _history;
	std::stack<state*> _future;
	boost::ptr_vector<std::string> _transitionList;
};


string prevKey(const Memory<Board>& memBrd) {
	if (!memBrd.Memory<Board>::TryUndo())
		return "";
	string result = memBrd.key();
	memBrd.Memory<Board>::TryRedo();
	return result;
}

inline void UndoAll(Memory<Board>& memBrd) {
	while (memBrd.TryUndo()) {}
}

inline void RedoAll(Memory<Board>& memBrd) {
	while (memBrd.TryRedo()) {}
}

