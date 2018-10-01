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
		while (this->depth() < index && this->next()) {}
		while (this->depth() > index && this->prev()) {}
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

	bool demote() {
		int d = this->depth();
		bool result = this->branch() && this->promote() && this->branch();
		this->set_to(d);
		return result;
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

	void snap_main() {
		while (this->snap()) {}
	}

	void branch_all() {
		while (this->branch()) {}
	}

	void branch_to(int index) {
		while (this->line() < index && this->branch()) {}
		while (this->line() > index && this->snap()) {}
	}

	void promote_main() {
		while (this->promote()) {}
	}

	void demote_last() {
		while (this->branch() && this->promote()) {}
	}

	void promote_to(int index) {
		while (this->line() > index && this->promote()) {}
		while (this->line() > index && this->demote()) {}
	}

	std::string show() {
		int d = this->depth();
		int l = this->line();
		this->snap_main();
		set_start();
		std::string result = this->show_sub();
		branch_to(l);
		set_to(d);
		return result.substr(result.find('-') + 1);
	}

	template<typename S>
	void load(S& stream) {
		std::string wrd;
		while (stream >> wrd) {
			if (wrd == "(") {
				stream >> wrd;
				this->prev();
				this->add(wrd);
			}
			else if (wrd == ")")
				this->snap();
			else
				this->add(wrd);
		}
	}

};

}
