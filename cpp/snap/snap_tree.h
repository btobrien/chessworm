
#pragma once
#include <stack>

template <typename T>
class snap_tree {
public:
	snap_tree(T init) : present(init), future(new std::stack<T>) {}

	~snap_tree() {
		delete future;
		while(!futures.empty()) {
			delete futures.top();
			futures.pop();
		}
	}

	inline T get() const { return present; }			

	void add(T val) {
		if (!future->empty()) {
			branch(val);
			return;
		}
		past.push(present);
		present = val;
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

	void branch(T val) {
		branch_points.push(depth());
		past.push(present);
		present = val;
		futures.push(future);
		future = new std::stack<T>;
		delete snap_future;
		snap_future = nullptr;
	}

	bool snap() {
		if (branch_points.empty())
			return false;
		set(*this, branch_points.top());
		branch_points.pop();
		snap_future = future;
		snap_point = depth();
		future = futures.top();
		futures.pop();
		return true;
	}

	// TODO: make multi level
	bool rebranch() {
		if (!snap_future)
			return false;
		set(*this, snap_point);
		std::stack<T>* new_future = snap_future;
		snap_future = nullptr;
		branch(new_future->top());
		new_future->pop();
		delete future;
		future = new_future;
		snap_future = nullptr;
		return true;
	}

	inline int depth() const { return past.size(); }
	inline int togo() const { return future->size(); }

private:
	std::stack<T> past;
	T present;
	std::stack<T>* future;
	std::stack<std::stack<T>*> futures;
	std::stack<int> branch_points;
	std::stack<T>* snap_future;
	int snap_point;
};


template <typename T>
inline void set_first(snap_tree<T>& st) {
	while (st.prev()) {}
}

template <typename T>
inline void set_last(snap_tree<T>& st) {
	while (st.next()) {}
}

template <typename T>
inline void set(snap_tree<T>& st, int index) {
	while (st.depth() < index && st.next()) {}
	while (st.depth() > index && st.prev()) {}
}

template <typename T>
inline void slide(snap_tree<T>& st, int dist) {
	set(st, st.depth() + dist);
}

