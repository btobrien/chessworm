#pragma once
#include "snap_tree_impl.h"

namespace Snap {

template <typename T>
class tree : public tree_impl<T> {
public:
	tree(T init) : tree_impl<T>(init) {}

	bool split(T val) {
		if (!this->prev())
			return false;
		this->add(val);
		return true;
	}

	void set_to(int index) {
		Snap::set_to(*this, index);
	}

	bool chop_branch() {
		if (this->snap()) {
			this->branch();
			this->chop();
			this->next();
			return true;
		}
		return false;
	}

	void set_start() {
		while (this->prev()) {}
	}

	void set_end() {
		while (this->next()) {}
	}

	void slide(int dist) {
		set_to(this->depth() + dist);
	}

	void snap_first() {
		while (this->snap()) {}
	}

	void branch_last() {
		while (this->branch()) {}
	}

	void branch_to(int index) {
		Snap::branch_to(*this, index);
	}

	void promote_first() {
		while (this->promote()) {}
	}

	template<typename S>
	void read(S& stream) {
		std::string wrd;
		while (stream >> wrd) {
			if (wrd == "(") {
				stream >> wrd;
				this->prev();
				this->add(wrd);
			}
			else if (wrd == ")")
				this->snap();
			else if (wrd == "/")
				set_start();
			else
				this->add(wrd);
		}
	}

	template<typename S>
	void show(S& stream) {
		int d = this->depth();
		int l = this->line();
		this->snap_first();
		do {
			set_start();
			while (this->next())
				stream << this->get() << " ";
			stream << "/ ";
		} while (this->branch());
		branch_to(l);
		set_to(d);
	}

};

}
