
#include <iostream>
#include <fstream>

#include <locale.h>
#include <ncurses.h>
#include "Board.cpp"

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
		case White::PAWN:      cout << whitePiece << background << width << "\u265F" << width; break;
		case White::KNIGHT:    cout << whitePiece << background << width << "\u265E" << width; break;
		case White::BISHOP:    cout << whitePiece << background << width << "\u265D" << width; break;
		case White::ROOK:      cout << whitePiece << background << width << "\u265C" << width; break;
		case White::QUEEN:     cout << whitePiece << background << width << "\u265B" << width; break;
		case White::KING:      cout << whitePiece << background << width << "\u265A" << width; break;
		case Black::PAWN:      cout << blackPiece << background << width << "\u265F" << width; break;
		case Black::KNIGHT:    cout << blackPiece << background << width << "\u265E" << width; break;
		case Black::BISHOP:    cout << blackPiece << background << width << "\u265D" << width; break;
		case Black::ROOK:      cout << blackPiece << background << width << "\u265C" << width; break;
		case Black::QUEEN:     cout << blackPiece << background << width << "\u265B" << width; break;
		case Black::KING:      cout << blackPiece << background << width << "\u265A" << width; break;
		case BoardState::nullpiece: cout << blackPiece << background << width << " "      << width; break;
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

	MemoryBoard board;
	bool isFlipped = false;
	Display(board);
	string input;

	while(true) {

		getline(cin, input);
		if (input == "f") {
			isFlipped = !isFlipped;
		}
		else if (input == "u") {
			board.TryUndoMove();
		}
		else if (input == "r") {
			board.TryRedoMove();
		}
		else if (!board.TryMove(input)) {
			Display(board, isFlipped);
			cout << "Move Failed: ";
			continue;
		}

		Display(board, isFlipped);
	}
}

