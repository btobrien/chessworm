
#include <iostream>
#include <fstream>

#include <locale.h>
#include <ncurses.h>
#include "Board.hpp"

using namespace std;

int main(int argc, const char * argv[]) {

	Board board;
	string input;

	while(true) {
		getline(cin, input);
		if (board.TryMove(input)) {
			for (int i = 0; i < 64; i++) {
				if (i % 8 == 0)
					cout << endl;
				cout << board.data()[i];
			}
		}
		cout << endl;
	}

}
