
#pragma once

#include "BoardState.h"
#include <stack>
#include <vector>
#include "Utils.cpp"
#include "Log.h"

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

	void Add(const std::string& transition) override {
		_history.push(copyState());
		if (!T::TryUpdate(transition)) {
			delete _history.top();
			_history.pop();
			return false;
		}	
		while (!_future.empty()) {
			delete _future.top();
			_future.pop();
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

	virtual bool TryRedo() { 
		if (_future.empty())
			return false;
		_history.push(SwapState(_future.top()));
		_future.pop();
		return true;
	}

private:
	std::stack<state*> _history;
	std::stack<state*> _future;
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


