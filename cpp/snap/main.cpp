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

	std::string init = "-";

	if (argc > 1) {
		init = argv[1];
	}
		
	Snap::tree<string> tree(init);

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
		else if (cmd == "demote")
			success = tree.demote();
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
		else if (cmd == "snap_main")
			tree.snap_main();
		else if (cmd == "branch_all")
			tree.branch_all();
		else if (cmd == "branch_to")
			tree.branch_to(stoi(getword(ss)));
		else if (cmd == "promote_main")
			tree.promote_main();
		else if (cmd == "demote_last")
			tree.demote_last();
		else if (cmd == "promote_to")
			tree.promote_to(stoi(getword(ss)));
		else if (cmd == "load")
			tree.load(ss);

		if (success) {
			cout << tree.get() << "  : ";
			cout << tree.show() << std::endl;
		}
	}
}

