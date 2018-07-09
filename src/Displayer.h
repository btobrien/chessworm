
struct dimension {
	int height;
	int width;
};

class IDisplayer {
	virtual bool tryDisplayTo(Window& win) = 0;
	virtual bool requiresDimension(const dimension& dim) { return false; } 
	virtual bool isValidDimension(dimension dim) { return true; } 
};

string MoveString(const AnnotatedMove& move) {
	return move.text + Glyph::ToString(move.glyph);
}
int FullClock(bool whiteToMove) {
	return (brd.clock() + 1) / 2;
}
int NextFullClock(bool whiteToMove) {
	return (brd.clock() + 2) / 2;
}
string MovePrefix(bool whiteToMove) { 
	string dot = whiteToMove ? ". " : "...";
	return std::to_string(NextFullClock(whiteToMove)) + dot; 
}

template <typename T>
class BoardDisplayerBase : Displayer {
public:

	BoardDisplayerBase(const& T brd) board(brd) {}

	virtual bool tryDisplayTo(Window& win) override {
		win.Reset();
		win.EnableAttribute(A_BOLD);
		for (int i = 0; i < BOARD_LENGTH; i++) {
			if (!isFlipped)
				DisplayRowTo(win, BOARD_LENGTH - i - 1);
			else
				DisplayFlippedRowTo(win, i);
		}
		win.DisableAttribute(A_BOLD);
	}

	void Flip() {
		isFlipped = !isFlipped;
	}

protected:
	static const int BOARD_LENGTH = 8;
private:

	const T& board;
	inline bool IsLightSquare(int row, int column) {
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

	bool _isFlipped;
	virtual void DisplayPieceTo(Window win, char piece, bool isLightSquare) = 0;
};


class ChoiceDisplayer : ChessDisplayer {
	ChoiceDisplayer(const Tree& tree) : Displayer {}

	virtual bool tryDisplayTo(Window& win) {
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

template <typename T>
class MoveHistoryDisplayer : Displayer {
	virtual bool tryDisplayTo(Window& win) {
		win.Clear();
		for (auto it = board.begin(); it != board.end(); it++) {
			Logger::log("ChessView::DisplayHead received " + it->text);
			win.AddChar(' ');
			DisplayMoveTo(win, *it);
		}
	}
private:
	const T& board
};

class CommandDisplayer : Displayer {
	virtual bool tryDisplayTo(Window& win) {
	}
private:
	string error;

};

class NullDisplayer : Displayer {
	virtual bool tryDisplayTo(Window& win) {
		return true;
	}
};




