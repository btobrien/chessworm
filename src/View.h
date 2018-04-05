template <class T>
class View {

	void SetSource(const T& sub) {
	}


	static void NcursesInit() {
		setlocale(LC_ALL, "");
		initscr();
		cbreak();
		noecho();
		keypad(stdscr, TRUE);
		start_color();
		set_escdelay(0);
	}

	void CalculateDimensions() {
		const int yBuffer = 1;
		const int xBuffer = 2;

		const int charsPerSquareHeight = 1;
		const int charsPerSquareWidth = 3;

		int height;
		int width;
		getmaxyx(stdscr, height, width);

		commandHeight = 1;
		commandWidth = width;
		int yCommand = height - commandHeight;
		int xCommand = 0;

		int boardHeight = charsPerSquareHeight * squaresPerCol;
		int boardWidth = charsPerSquareWidth * squaresPerRow;
		int xBoard = xBuffer;
		int yBoard = yCommand - yBuffer - boardHeight;

		headHeight = 1;
		headWidth = width - (2 * xBuffer);
		int yHead = yBoard - headHeight - yBuffer;
		int xHead = xBuffer;

		int xText = xBoard + boardWidth + (2 * xBuffer);
		int yText = yBoard;
		textWidth = width - xText - xBuffer;
		textHeight = yCommand - yBuffer - yText;

		boardWin = newwin(boardHeight, boardWidth, yBoard, xBoard);
		headWin = newwin(headHeight, headWidth, yHead, xHead);
		commandWin = newwin(commandHeight, commandWidth, yCommand, xCommand);
		textWin = newwin(textHeight, textWidth, yText, xText);
	}
};
