namespace StaticView {

dimension dimension() {
	dimension dim;
	getmaxyx(stdscr, dim.height, dim.width);
	return dim;
}

void NcursesInit() {
	setlocale(LC_ALL, "");
	initscr();
	cbreak();
	noecho();
	keypad(stdscr, TRUE);
	start_color();
	set_escdelay(0);

	init_pair(ColorCombo::WhiteOnLight, COLOR_WHITE, COLOR_CYAN); 
	init_pair(ColorCombo::WhiteOnDark, COLOR_WHITE, COLOR_BLUE); 
	init_pair(ColorCombo::BlackOnLight, COLOR_BLACK, COLOR_CYAN); 
	init_pair(ColorCombo::BlackOnDark, COLOR_BLACK, COLOR_BLUE); 
}

class View : EchoWindow {

	void SetTopDisplayer(const Displayer& disp, int priority = 0);
	void SetLeftDisplayer(const Displayer& disp, int priority = 0);
	void SetRightDisplayer(const Displayer& disp, int priority = 0);
	void SetBottomDisplayer(const Displayer& disp, int priority = 0);

	bool TryFit();

	void Flash() {
		flash();
	}

private:
	Display topDisp;
	Display leftDisp;
	Display rightDisp;
	Display bottomDisp;

	void CalculateFrames() {
		const dimension buffer;
		buffer.x = 1;
		buffer.y = 2;
		// initializer lists??

		dimension total = dimension();

		frame command;
		command.height = 1;
		command.width = total.width;
		command.y = total.height - command.height;
		command.x = 0;
		

		frame left;
		left.height = charsPerSquareHeight * squaresPerCol;
		left.width = charsPerSquareWidth * squaresPerRow;
		left.x = bufferX;
		left.y = command.y - buffer.y - left.height;

		frame top;
		top.height = 1;
		top.width = total.width - (2 * buffer.x);
		top.y = left.y - top.height - buffer.y;
		top.x = buffer.x;

		frame right;
		right.x = left.x + left.width + (2 * buffer.x);
		right.y = left.y;
		right.width = total.width - right.x - buffer.x;
		right.height = command.y - buffer.y - right.y;

		//still need bottom frame

		leftWin = newwin(boardHeight, boardWidth, boardY, boardX);
		topWin = newwin(headHeight, headWidth, yHead, xHead);
		commandWin = newwin(commandHeight, commandWidth, yCommand, xCommand);
		textWin = newwin(textHeight, textWidth, yText, xText);
	}

};

}
