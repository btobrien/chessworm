#include "snap_tree.h"
#include "include/read.h"
#include <sstream>
#include <iostream>

using std::string;
using std::stringstream;
using std::cin;
using std::cout;
using std::cerr;
using std::endl;
using std::fstream;

int main(int argc, char* argv[]) {

	Snap::tree<string> tree("-");

	if (argc > 1) {
		stringstream ss(argv[1]);
		tree.read(ss);
	}

	cout << tree.get() << " : ";
	tree.show(cout);
	cout << std::endl;

	string line;
	while(getline(cin, line)) {
		stringstream ss(line);
		string cmd = getword(ss);
		bool success = true;

		if (cmd == "add")
			tree.add(getword(ss));
		else if (cmd == "next")
			success = tree.next();
		else if (cmd == "prev")
			success = tree.prev();
		else if (cmd == "snap")
			success = tree.snap();
		else if (cmd == "branch")
			success = try_getword(ss, cmd) ? tree.split(cmd) : tree.branch();
		else if (cmd == "promote")
			success = tree.promote();
		else if (cmd == "chop")
			success = tree.chop();
		else if (cmd == "chop_branch")
			success = tree.chop_branch();
		else if (cmd == "start")
			tree.set_start();
		else if (cmd == "end")
			tree.set_end();
		else if (cmd == "set")
			tree.set_to(stoi(getword(ss)));
		else if (cmd == "slide")
			tree.slide(stoi(getword(ss)));
		else if (cmd == "snap_first")
			tree.snap_first();
		else if (cmd == "branch_last")
			tree.branch_last();
		else if (cmd == "branch_to")
			tree.branch_to(stoi(getword(ss)));
		else if (cmd == "promote_first")
			tree.promote_first();
		else if (cmd == "read")
			tree.read(ss);

		if (success) {
			cout << tree.get() << " : ";
			tree.show(cout);
			cout << " : ";
			cout << tree.line() << " : ";
			cout << tree.depth() << std::endl;
		}
	}
}

