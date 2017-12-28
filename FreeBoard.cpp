
#include <iostream>
#include <fstream>

#include <locale.h>
#include <ncurses.h>
#include "Board.h"

using namespace std;

void ClearScreen() {
	cout << string(10, '\n');
}

string ToUnicode(char p, int row, int column) {
	bool isDarkSquare = ((row + column) % 2) == 0;
	switch(p) {
		case 'P': return " \u2659";
		case 'N': return " \u2658";
		case 'B': return " \u2657";
		case 'R': return " \u2656";
		case 'Q': return " \u2655";
		case 'K': return " \u2654";
		case 'p': return " \u265F";
		case 'n': return " \u265E";
		case 'b': return " \u265D";
		case 'r': return " \u265C";
		case 'q': return " \u265B";
		case 'k': return " \u265A";
		default: return isDarkSquare ?  " -" : " -";
	}
}

void DisplayRow(Board board, int row) {
	for (int j = 0; j < 8; j++)
		cout << ToUnicode(board[row * 8 + j], row, j);
}

void Display(Board board) {
	ClearScreen();
	for (int i = 7; i >= 0; i--) {
		DisplayRow(board, i);
		cout << endl;
	}
}

int main(int argc, const char * argv[]) {

	cout << endl;

	Board board;
	Display(board);

	string input;

	while(true) {
		getline(cin, input);
		if (board.TryMove(input)) {
			Display(board);
		}
		else {
			Display(board);
			cout << "Move failed";
		}
	}

}


