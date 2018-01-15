
#include <iostream>
#include <fstream>

#include <locale.h>
#include <ncurses.h>
#include "Board.h"

using namespace std;

void ClearScreen() {
	cout << string(10, '\n') << endl;
}

void DisplayPiece(char p, int row, int column) {

	string whitePiece = "\033[97";
	string blackPiece = "\033[30";
	string background = (row + column) % 2 ? ";47m" : ";44m";
	string width = " ";

	switch(p) {
		case 'P': cout << whitePiece << background << width << "\u265F" << width; break;
		case 'N': cout << whitePiece << background << width << "\u265E" << width; break;
		case 'B': cout << whitePiece << background << width << "\u265D" << width; break;
		case 'R': cout << whitePiece << background << width << "\u265C" << width; break;
		case 'Q': cout << whitePiece << background << width << "\u265B" << width; break;
		case 'K': cout << whitePiece << background << width << "\u265A" << width; break;
		case 'p': cout << blackPiece << background << width << "\u265F" << width; break;
		case 'n': cout << blackPiece << background << width << "\u265E" << width; break;
		case 'b': cout << blackPiece << background << width << "\u265D" << width; break;
		case 'r': cout << blackPiece << background << width << "\u265C" << width; break;
		case 'q': cout << blackPiece << background << width << "\u265B" << width; break;
		case 'k': cout << blackPiece << background << width << "\u265A" << width; break;
		case  0 : cout << blackPiece << background << width << " "      << width; break;
	}
}

void DisplayRow(Board board, int row) {
	for (int column = 0; column < 8; column++) {
		DisplayPiece(board[row * 8 + column], row, column);
	}
	cout << "\033[0m";
}

void DisplayRowFlipped(Board board, int row) {
	for (int column = 7; column >= 0; column--) {
		DisplayPiece(board[row * 8 + column], row, column);
	}
	cout << "\033[0m";
}

void Display(Board board, bool isFlipped = false) {
	ClearScreen();
	for (int i = 0; i < 8; i++) {
		cout << " ";
		if (!isFlipped)
			DisplayRow(board, 7 - i);
		else
			DisplayRowFlipped(board, i);
		cout << endl;
	}
}

int main(int argc, const char* argv[]) {

	cout << endl;

	Board board;
	bool isFlipped = false;
	Display(board, isFlipped);
	string input;

	while(true) {
		getline(cin, input);
		if (input == "f") {
			isFlipped = !isFlipped;
			continue;
		}
		if (!board.TryMove(input)) {
			Display(board, isFlipped);
			cout << "Move Failed: ";
			continue;
		}
		Display(board, isFlipped);
	}
}

