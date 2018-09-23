#include <string>
#include "snap_tree.h"
#include "include/read.h"

using namespace std;

int main(int argc, char* argv[]) {

	string init = "-";
	if (argc > 2)
		init = argv[1];
		
	snap_tree<string> snap(init);

	string line;
	while(getline(cin, line)) {
		string cmd = getword(line);

		if (cmd == "print")
			cout << snap.get() << endl;
		else if (cmd == "add")
			snap.add(getword(line));
		else if (cmd == "next")
			snap.next();
		else if (cmd == "prev")
			snap.prev();
		else if (cmd == "branch")
			snap.branch(getword(line));
		else if (cmd == "snap")
			snap.snap();
		else if (cmd == "rebranch")
			snap.rebranch();
		else if (cmd == "first")
			set_first(snap);
		else if (cmd == "last")
			set_last(snap);
		else if (cmd == "set")
			set(snap, stoi(getword(line)));
		else if (cmd == "slide")
			slide(snap, stoi(getword(line)));
		else if (cmd == "all") {
			int depth = snap.depth();
			set_first(snap);
			do {
				cout << snap.get() << " ";
			} while (snap.next());
			cout << endl;
			set(snap, depth);
		}
	}
}
