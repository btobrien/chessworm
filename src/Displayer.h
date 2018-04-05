
class IDisplayer {
	virtual TryDisplayTo(Window& win) = 0;
};


string MoveString(const AnnotatedMove& move) {
	return move.text + Glyph::ToString(move.glyph);
}
int FullClock(const Board& brd) {
	return (brd.clock() + 1) / 2;
}
int NextFullClock(const Board& brd) {
	return (brd.clock() + 2) / 2;
}
string MovePrefix(const Board& brd) { 
	string dot = brd.whiteToMove() ? ". " : "...";
	return std::to_string(NextFullClock(brd)) + dot; 
}


template <typename T>
class BoardDisplayer : Displayer {
public:
	BoardDisplayer(const& T brd) {
		Window.InitPair(ColorCombo::WhiteOnLight, COLOR_WHITE, COLOR_CYAN); 
		Window.InitPair(ColorCombo::WhiteOnDark, COLOR_WHITE, COLOR_BLUE); 
		Window.InitPair(ColorCombo::BlackOnLight, COLOR_BLACK, COLOR_CYAN); 
		Window.InitPair(ColorCombo::BlackOnDark, COLOR_BLACK, COLOR_BLUE); 
	}

	virtual bool TryDisplayTo(Window& win) override {
		win.Reset();
		win.AddAttribute(A_BOLD);
		for (int i = 0; i < SQUARES_PER_ROW; i++) {
			if (!isFlipped)
				DisplayRowTo(win, SQUARES_PER_ROW - i - 1);
			else
				DisplayFlippedRowTo(win, i);
		}
		win.RemoveAttribute(A_BOLD);
	}

	void FlipBoard() {
		isFlipped = !isFlipped;
	}

protected:
	const T& board;
	static const int SQUARES_PER_ROW = 8;
	enum ColorCombo { WhiteOnLight = 1, WhiteOnDark = 2, BlackOnLight = 3, BlackOnDark = 4 };

	void DisplayPieceTo(Window win, char piece, bool isLightSquare) {
		string buffer = " ";
		bool isWhitePiece = White::IsMyPiece(piece);
		int colorPair;

		if      ( isWhitePiece &&  isLightSquare) colorPair = ColorCombo::WhiteOnLight;
		else if ( isWhitePiece && !isLightSquare) colorPair = ColorCombo::WhiteOnDark;
		else if (!isWhitePiece &&  isLightSquare) colorPair = ColorCombo::BlackOnLight;
		else if (!isWhitePiece && !isLightSquare) colorPair = ColorCombo::BlackOnDark;
		
		win.AddAttribute(COLOR_PAIR(colorPair));
		win.AddString(buffe); 

		switch(piece) {
			case White::PAWN:		    win.AddString("\u265F"); break;
			case White::KNIGHT:		    win.AddString("\u265E"); break;
			case White::BISHOP:		    win.AddString("\u265D"); break;
			case White::ROOK:		    win.AddString("\u265C"); break;
			case White::QUEEN:		    win.AddString("\u265B"); break;
			case White::KING:		    win.AddString("\u265A"); break;
			case Black::PAWN:		    win.AddString("\u265F"); break;
			case Black::KNIGHT:		    win.AddString("\u265E"); break;
			case Black::BISHOP:		    win.AddString("\u265D"); break;
			case Black::ROOK:		    win.AddString("\u265C"); break;
			case Black::QUEEN:		    win.AddString("\u265B"); break;
			case Black::KING:		    win.AddString("\u265A"); break;
			default:					win.AddString(" ");      break;
		}

		win.AddString(buffer); 
		win.RemoveAttribute(COLOR_PAIR(colorPair));
	}

	bool IsLightSquare(int row, int column) {
		return (row + column) % 2;
	}

	void DisplayRowTo(Win* win, int row) {
		for (int column = 0; column < squaresPerRow; column++) {
			DisplayPieceTo(win, source[row * squaresPerRow + column], IsLightSquare(row, column));
		}
	}

	void DisplayFlippedRowTo(Win* win, int row) {
		for (int column = squaresPerRow - 1; column >= 0; column--) {
			DisplayPieceTo(win, source[row * squaresPerRow + column], IsLightSquare(row, column));
		}
	}

private:
	bool isFlipped;
};

class ChoiceDisplayer : ChessDisplayer {
	ChoiceDisplayer(const Tree& tree) : ChessDisplayer(tree) {}

	virtual bool TryDisplayTo(Window& win) {
		win.Reset();
		for (auto it = tree.begin(); it != tree.end(); it++) {
			Logger::log("ChessView::DisplayHead received " + it->text);
			win.AddChar(' ');
			DisplayMoveTo(win, *it);
		}
	}
private:
	const Tree& tree;
};

class MoveHistoryDisplayer : ChessDisplayer {
	virtual bool TryDisplayTo(Window& win) {
		win.Reset();
		for (auto it = board.MemoryBoard::begin(); it != board.MemoryBoard::end(); it++) {
			Logger::log("ChessView::DisplayHead received " + it->text);
			win.AddChar(' ');
			DisplayMoveTo(win, *it);
		}
	}
private:
	const Memory<Board>& board
};

class CommandDisplayer : ChessDisplayer {
	virtual bool TryDisplayTo() {
	}

};

class NullDisplayer : Displayer {
	virtual bool TryDisplayTo(Window& win) {
		return true;
	}

};




