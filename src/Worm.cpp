
#pragma once
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <vector>
#include <fstream>
#include <locale.h>
#include <ncurses.h>
#include "TreeBoard.cpp"
#include "Parse.h"
#include "Glyph.h"
#include "Log.h"

using std::string;
using std::vector;
using std::ifstream;

class Worm : View<Board> {

public:
    template <typename GameContainer>
    ChessView(const GameContainer& games) : board(games), isFlipped(false) {
		Logger::log("ChessView::Constructor");
		NcursesInit();
	}

	~ChessView() {
		endwin();
	}

	bool TryReadInput() {
		char input = getch();
		string move;

		switch (input) {
			case 'f':
				boardDisplay.FlipBoard();
				break;
			case 'g':
				input = getch();
				if (input = 'g')
					while(board.TryUndo()) {}
				break;
			case 'G':
				while(board.TryRedo()) {}
				break;
			case '/':
				DisplayMovePrompt();
				move = GetEcho(commandWin);
				if (!board.TryMove(move)) {
					flash();
				}
				break;
			case 'j':
				if (!board.TryRedo())
					flash();
				break;
			case 'k':

			case 'N':
				if (!board.TryUndo())
					flash();
				break;
			case 'n':
				if (!board.TryRedo())
					flash();
				break;
			case ':':
				werase(commandWin);
				wmove(commandWin, 0, 0);
				waddch(commandWin, ':');
				wrefresh(commandWin);
				if (GetEcho(commandWin) == "q")
					return false;
		}
		return true;
	}

protected:
	TreeBoard board;

	void DisplayMovePrompt() {
		ClearWin(commandWin);
		waddstr(commandWin, CommandPrefix().data());
		wrefresh(commandWin);
	}

	void DisplayHead() {
		ClearWin(headWin);
		board.GetNextMoves(moveChoices);
		Logger::log("ChessView::DisplayHead received "
		+ std::to_string(moveChoices.size()) + " move choices:");
		for (auto move : moveChoices) {
			Logger::log("ChessView::DisplayHead received " + move->text);
			waddch(headWin, ' ');
			DisplayMove(headWin, *move);
		}
		wrefresh(headWin);
	}

	bool DisplayTextLine(int fullMoveNumber) {
		int moveIndex = (fullMoveNumber - 1) * 2;

		if (moveIndex < 0 || moveIndex >= moveHistory.size())
			return false;

		waddstr(textWin, std::to_string(fullMoveNumber).data());
		waddstr(textWin, ". ");
		waddstr(textWin, moveHistory[moveIndex]->data());

		if (moveIndex + 1 >= moveHistory.size())
			return false;

		waddch(textWin, ' ');
		waddstr(textWin, moveHistory[moveIndex + 1]->data());
	}

	void DisplayText() {
		ClearWin(textWin);
		board.GetMoveHistory(moveHistory);
		for (int i = 0; i < board.clock(); i++) {
			DisplayTextLine(i);
			waddch(textWin, '\n');
		}
		DisplayTextLine(FullClock());
		wrefresh(textWin);
	}

};

int main(int argc, const char* argv[]) {
	ifstream pgn;
	Logger::open();
	pgn.open(argv[1]);
	if (!pgn.is_open()) {
		std::cerr << "ERROR: failed to open " << argv[1];
		return 1;
	}
	Logger::log("main: succesfully logged");
	Logger::file() << "main: succesfully opened " << argv[1] << std::endl;
	vector<Game*> games;
		games.push_back(new Game(pgn));
	pgn.close();
	Worm worm(games);
	Logger::log("main is starting input loop");
	while(worm.TryReadInput()) {
	
		Logger::log("waiting for input");
	}
	Logger::close();
}

