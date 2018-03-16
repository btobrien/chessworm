
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <vector>
#include <fstream>

#include <locale.h>
#include <ncurses.h>
#include "TreeBoard.cpp"
#include "Parse.cpp"

using std::string;
using std::vector;
using std::ifstream;


class ChessView {

public:
    template <typename GameContainer>
    ChessView(const GameContainer& games) : board(games), ChessView() {}

	ChessView() : isFlipped(false) {
		setlocale(LC_ALL, "");
		initscr();
		cbreak();
		noecho();
		keypad(stdscr, TRUE);
		start_color();
		set_escdelay(0);
		scrollok(textWin, true);
	
		init_pair(ColorCombo::WhiteOnLight, COLOR_WHITE, COLOR_CYAN); 
		init_pair(ColorCombo::WhiteOnDark, COLOR_WHITE, COLOR_BLUE); 
		init_pair(ColorCombo::BlackOnLight, COLOR_BLACK, COLOR_CYAN); 
		init_pair(ColorCombo::BlackOnDark, COLOR_BLACK, COLOR_BLUE); 

		CalculateWinDimensions();
		wmove(commandWin, 0, 0);
	}

	~ChessView() {
		delwin(headWin);
		delwin(boardWin);
		delwin(textWin);
		delwin(commandWin);
		endwin();
	}

	bool TryReadInput() {
		char input = getch();
		string move;

		switch (input) {

			case 'f':
				FlipBoard();
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
					break;
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

		Display();
		return true;
	}

protected:
	TreeBoard board;
	bool isFlipped;
	vector<const string*> moveHistory;
	vector<AnnotatedMove*> nextMoves;

	static const int squaresPerRow = 8;
	static const int squaresPerCol = 8;

	enum ColorCombo { WhiteOnLight = 1, WhiteOnDark = 2, BlackOnLight = 3, BlackOnDark = 4 };

	int textHeight;
	int textWidth;
	int headHeight;
	int headWidth;
	int commandHeight;
	int commandWidth;

	WINDOW* headWin;
	WINDOW* boardWin;
	WINDOW* textWin;
	WINDOW* commandWin;

	void CalculateWinDimensions() {
		int yBuffer = 1;
		int xBuffer = 2;

		int charsPerSquareHeight = 1;
		int charsPerSquareWidth = 3;

		int height;
		int width;
		getmaxyx(stdscr, height, width);

		headHeight = 1;
		headWidth = width - 2 * xBuffer;
		int yHead = yBuffer;
		int xHead = xBuffer;

		commandHeight = 1;
		commandWidth = width;
		int yCommand = height - commandHeight;
		int xCommand = 0;

		int boardHeight = charsPerSquareHeight * squaresPerCol;
		int boardWidth = charsPerSquareWidth * squaresPerRow;
		int xBoard = xBuffer;
		int yBoard = yCommand - yBuffer - boardHeight;

		int xText = xBoard + boardWidth + 2 * xBuffer;
		int yText = yBoard;
		textWidth = width - xText - xBuffer;
		textHeight = yCommand - yBuffer - yText;

		boardWin = newwin(boardHeight, boardWidth, yBoard, xBoard);
		headWin = newwin(headHeight, headWidth, yHead, xHead);
		commandWin = newwin(commandHeight, commandWidth, yCommand, xCommand);
		textWin = newwin(textHeight, textWidth, yText, xText);
	}
		
	void FlipBoard() {
		isFlipped = !isFlipped;
	}

	void DisplayPiece(char piece, int row, int column) {
		string width = " ";
		bool isWhitePiece = White::IsMyPiece(piece);
		bool isLightSquare = (row + column) % 2;

		int colorPair;

		if      ( isWhitePiece &&  isLightSquare) colorPair = ColorCombo::WhiteOnLight;
		else if ( isWhitePiece && !isLightSquare) colorPair = ColorCombo::WhiteOnDark;
		else if (!isWhitePiece &&  isLightSquare) colorPair = ColorCombo::BlackOnLight;
		else if (!isWhitePiece && !isLightSquare) colorPair = ColorCombo::BlackOnDark;
		
		wattron(boardWin, COLOR_PAIR(colorPair));
		waddstr(boardWin, width.data()); 

		switch(piece) {
			case White::PAWN:		    waddstr(boardWin, "\u265F"); break;
			case White::KNIGHT:		    waddstr(boardWin, "\u265E"); break;
			case White::BISHOP:		    waddstr(boardWin, "\u265D"); break;
			case White::ROOK:		    waddstr(boardWin, "\u265C"); break;
			case White::QUEEN:		    waddstr(boardWin, "\u265B"); break;
			case White::KING:		    waddstr(boardWin, "\u265A"); break;
			case Black::PAWN:		    waddstr(boardWin, "\u265F"); break;
			case Black::KNIGHT:		    waddstr(boardWin, "\u265E"); break;
			case Black::BISHOP:		    waddstr(boardWin, "\u265D"); break;
			case Black::ROOK:		    waddstr(boardWin, "\u265C"); break;
			case Black::QUEEN:		    waddstr(boardWin, "\u265B"); break;
			case Black::KING:		    waddstr(boardWin, "\u265A"); break;
			case BoardState::nullpiece: waddstr(boardWin, " ");      break;
		}

		waddstr(boardWin, width.data()); 
		wattroff(boardWin, COLOR_PAIR(colorPair));

	}

	void DisplayRow(int row) {
		for (int column = 0; column < squaresPerRow; column++) {
			DisplayPiece(board[row * squaresPerRow + column], row, column);
		}
	}

	void DisplayRowFlipped(int row) {
		for (int column = squaresPerRow - 1; column >= 0; column--) {
			DisplayPiece(board[row * squaresPerRow + column], row, column);
		}
	}

	void ClearWin(Win* win) {
		werase(win);
		wmove(win, 0, 0);
	}

	void DisplayBoard() {
		wmove(boardWin, 0, 0);
		wattron(boardWin, A_BOLD);
		for (int i = 0; i < squaresPerCol; i++) {
			if (!isFlipped)
				DisplayRow(squaresPerCol - i - 1);
			else
				DisplayRowFlipped(i);
		}
		wattroff(boardWin, A_BOLD);
		wrefresh(boardWin);
	}

	void DisplayMovePrompt() {
		ClearWin(commandWin);
		waddstr(commandWin, CommandPrefix().data());
		wrefresh(commandWin);
	}

	void DisplayMove(Win* win, const AnnotatedMove&) {
		waddstr(win, move->comment->data());
		waddstr(win, move->Game::NAG[glyph]);
	}

	void DisplayHead() {
		ClearWin(headWin);
		board.GetNextMoves(nextMoves);
		for (auto move : nextMoves) {
			waddch(headWin, ' ');
			DisplayMove(headWin, *move);
		}
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

	void DisplayCommand() {
		werase(commandWin);
		wrefresh(commandWin);
	}

	void Display() {
		DisplayHead();
		DisplayBoard();
		DisplayText();
		DisplayCommand();
	}

	int FullClock() {
		return (board.clock() + 1) / 2;
	}

	int NextFullClock() {
		return (board.clock() + 2) / 2;
	}

	string CommandPrefix() { 
		string dot = board.whiteToMove() ? ". " : "...";
		return std::to_string(NextFullClock()) + dot; 
	}

	bool IsDeleteChar(char input) { return input == KEY_BACKSPACE || input == KEY_DC || input == 127 || input == 8; }

	void BackSpace(WINDOW* win) {
		int x, y;
		getyx(commandWin, y, x);
		wmove(commandWin, y, x - 1);
		wdelch(win);
	}

	string GetEcho(WINDOW* win) {
		string result;
		char input = getch();
		while(input != '\n' && input != KEY_ENTER) {
			if (input == 27)
				return "";
			if (IsDeleteChar(input)) {
				if (!result.empty()) {
					result.pop_back();
					BackSpace(commandWin);
				}
			}
			else  {
				waddch(win, input);
				result += input;
			}
			wrefresh(commandWin);
			input = getch();
		}
		return result;
	}

};

int main(int argc, const char* argv[]) {
	ifstream pgn;
	pgn.open(argv[1]);
	if (!pgn.is_open()) {
		std::cerr << "ERROR: failed to open " << argv[1];
		return 1;
	}
	vector<Game*> games;
	while(!pgn.eof())
		games.push_back(new Game(pgn));
	pgn.close();
	ChessView view(games);
	while(view.TryReadInput()) {}
}

