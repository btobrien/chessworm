#pragma once
#include <stack>
#include <iostream>
#include <sstream>

namespace Snap {

template <typename T>
class tree_impl {
public:
	tree_impl(T init) : present(init), future(new std::stack<T>) {}

	~tree_impl() {
		set_to(*this, 1);
		chop();
		delete future;
	}

	inline T get() const { return present; }			

	void add(T val) {
		if(future->empty()) {
			past.push(present);
			present = val;
			return;
		}
		if (find_repeat(val))
			return;
		new_branch(val);
	}			

	bool prev() { 
		if (past.empty())
			return false;
		future->push(present);
		present = past.top();
		past.pop();
		return true;
	}

	bool next() { 
		if (future->empty())
			return false;
		past.push(present);
		present = future->top();
		future->pop();
		return true;
	}

	bool snap() {
		if (snap_points.empty())
			return false;
		set_to(*this, snap_points.top());
		snap_points.pop();
		branch_points.push(depth());
		branches.push(future);
		future = snaps.top();
		snaps.pop();
		next();
		return true;
	}

	bool branch() {
		if (branch_points.empty())
			return false;
		set_to(*this, branch_points.top());
		branch_points.pop();
		snap_points.push(depth());
		snaps.push(future);
		future = branches.top();
		branches.pop();
		next();
		return true;
	}

	bool promote() {
		// TODO: fix or rm...
		if (line() == 0)
			return false;
		branch_points.push(snap_points.top());
		snap_points.pop();
		branches.push(snaps.top());
		snaps.pop();
		return true;
	}

	bool chop() {
		if (!prev())
			return false;
		delete future;
		while (!snap_points.empty() && snap_points.top() > depth()) {
			snap_points.pop();
			delete snaps.top();
			snaps.pop();
		}
		while (!branch_points.empty() && branch_points.top() > depth()) {
			branch_points.pop();
			delete branches.top();
			branches.pop();
		}
		if (!snaps.empty() && snap_points.top() == depth()) {
			future = snaps.top();
			snaps.pop();
			snap_points.pop();
		}
		else if (!branches.empty() && branch_points.top() == depth()) {
			future = branches.top();
			branches.pop();
			branch_points.pop();
		}
		else {
			future = new std::stack<T>;
		}
		return true;
	}

	inline int depth() const { return past.size(); }
	inline int line() const { return snaps.size(); }

private:
	std::stack<T> past;
	T present;
	std::stack<T>* future;
	std::stack<std::stack<T>*> snaps;
	std::stack<int> snap_points;
	std::stack<std::stack<T>*> branches;
	std::stack<int> branch_points;

	void new_branch(T val) {
		snap_points.push(depth());
		past.push(present);
		present = val;
		snaps.push(future);
		future = new std::stack<T>;
	}
	
	bool find_repeat(T val) {
		if (val == future->top()) {
			next();
			return true;
		}
		int d = depth();
		while (!branches.empty() && d < branch_points.top()) {
			branch();
		}
		while (!snap_points.empty() && d == snap_points.top()) {
			snap();
		}
		if (val == get())
			return true;
		while (!branches.empty() && d == branch_points.top()) {
			branch();
			if (val == get())
				return true;
		}
		set_to(*this, d);
		return false;
	}
};

template<typename T>
void set_to(T& tree, int index) {
	while (tree.depth() < index && tree.next()) {}
	while (tree.depth() > index && tree.prev()) {} 
}

template<typename T>
void branch_to(T& tree, int index) {
	while (tree.line() < index && tree.branch()) {}
	while (tree.line() > index && tree.snap()) {}
}

}
