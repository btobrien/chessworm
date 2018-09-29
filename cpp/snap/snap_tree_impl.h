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
		if (branch_points.empty())
			return false;
		set_to(*this, branch_points.top());
		branch_points.pop();
		snaps.push(future);
		snap_points.push(depth());
		future = branches.top();
		branches.pop();
		next();
		return true;
	}

	bool branch() {
		if (snap_points.empty())
			return false;
		set_to(*this, snap_points.top());
		snap_points.pop();
		branch_points.push(depth());
		past.push(present);
		branches.push(future);
		future = snaps.top();
		snaps.pop();
		present = future->top();
		future->pop();
		return true;
	}

	bool promote() {
		if (line() == 0)
			return false;
		snaps.push(branches.top());
		branches.pop();
		snap_points.push(branch_points.top());
		branch_points.pop();
		return true;
	}

	bool chop() {
		if (depth() == 0)
			return false;
		prev();
		delete future;
		while (!branch_points.empty() && branch_points.top() > depth()) {
			branch_points.pop();
			delete branches.top();
			branches.pop();
		}
		while (!snap_points.empty() && snap_points.top() > depth()) {
			snap_points.pop();
			delete snaps.top();
			snaps.pop();
		}
		if (!branches.empty() && branch_points.top() == depth()) {
			future = branches.top();
			branches.pop();
			branch_points.pop();
		}
		else if (!snaps.empty() && snap_points.top() == depth()) {
			future = snaps.top();
			snaps.pop();
			snap_points.pop();
		}
		else {
			future = new std::stack<T>;
		}
		return true;
	}

	inline int depth() const { return past.size(); }
	inline int line() const { return branches.size(); }

protected:

	std::string show_sub() {
		std::stringstream ss;
		ss << " " << get();
		if (snaps.empty() || depth() > snap_points.top()) {
			while (next())
				ss << " " << get();
			return ss.str();
		}
		while (depth() <= snap_points.top()) {
			next();
			ss << " " << get();
		}
		branch();
		ss << " (" << show_sub() << " )";
		snap();
		while (next())
			ss << " " << get();
		return ss.str();
	}
	
private:
	std::stack<T> past;
	T present;
	std::stack<T>* future;
	std::stack<std::stack<T>*> branches;
	std::stack<int> branch_points;
	std::stack<std::stack<T>*> snaps;
	std::stack<int> snap_points;

	void new_branch(T val) {
		branch_points.push(depth());
		past.push(present);
		present = val;
		branches.push(future);
		future = new std::stack<T>;
	}
	
	bool find_repeat(T val) {
		if (val == future->top()) {
			next();
			return true;
		}
		int d = depth();
		while (!snaps.empty() && d < snap_points.top()) {
			branch();
		}
		while (!branch_points.empty() && d == branch_points.top()) {
			snap();
		}
		if (val == get())
			return true;
		while (!snaps.empty() && d == snap_points.top()) {
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

}
