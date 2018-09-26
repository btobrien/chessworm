#include "snap_tree.h"
#include "include/read.h"

using std::string;
using std::stringstream;
using std::cin;
using std::cout;
using std::cerr;
using std::endl;

int main(int argc, char* argv[]) {

	string init = "-";
	if (argc > 2)
		init = argv[1];
		
	snap_tree<string> snap(init);

	string line;
	while(getline(cin, line)) {
		stringstream ss(line);
		string cmd = getword(ss);
		bool success = true;

		if (cmd == "add")
			snap.add(getword(ss));
		else if (cmd == "next")
			success = snap.next();
		else if (cmd == "prev")
			success = snap.prev();
		else if (cmd == "branch")
			snap.branch(getword(ss));
		else if (cmd == "snap")
			success = snap.snap();
		else if (cmd == "rebranch")
			success = snap.rebranch();
		else if (cmd == "first")
			set_first(snap);
		else if (cmd == "last")
			set_last(snap);
		else if (cmd == "set")
			set(snap, stoi(getword(ss)));
		else if (cmd == "slide")
			slide(snap, stoi(getword(ss)));

		if (!success)
			cerr << "\a";
		else
			cout << snap.get() << endl;
	}
}
