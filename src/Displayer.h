#include "ColorCombo.h"

struct dimension {
	int height;
	int width;
};

class IDisplayer {
	virtual bool TryDisplayTo(Window& win) = 0;
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
class BoardDisplayer : Displayer {
public:
	BoardDisplayer(const& T brd) board(brd) {
	}

	virtual bool TryDisplayTo(Window& win) override {
		win.Reset();
		win.EnableAttribute(A_BOLD);
		for (int i = 0; i < SQUARES_PER_ROW; i++) {
			if (!isFlipped)
				DisplayRowTo(win, SQUARES_PER_ROW - i - 1);
			else
				DisplayFlippedRowTo(win, i);
		}
		win.DisableAttribute(A_BOLD);
	}

	virtual bool requiresDimension(const dimension& dim) {
		dim.height = SQUARES_PER_ROW;
		dim.width = reqWidth();
		return true;
	}

	virtual bool IsValidDimension(dimension dim) {
		return dim.height >= SQUARES_PER_ROW && dim.width == regWidth();
	}

	void Flip() {
		isFlipped = !isFlipped;
	}

protected:
	const T& board;
	static const int SQUARES_PER_ROW = 8;
	static const int SQUARE_BUFFER = 1;
	// use const expr
	static const reqWidth() { return (1 + (2 * SQUARE_BUFFER)) * 8; }
	void DisplayPieceTo(Window win, char piece, bool isLightSquare) {
		string buffer = string(" ", SQUARE_BUFFER);
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
private:
	bool _isFlipped;
};


class ChoiceDisplayer : ChessDisplayer {
	ChoiceDisplayer(const Tree& tree) : Displayer {}

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

template <typename T>
class MoveHistoryDisplayer : Displayer {
	virtual bool TryDisplayTo(Window& win) {
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
	virtual bool TryDisplayTo(Window& win) {
	}
private:
	string error;

};

class NullDisplayer : Displayer {
	virtual bool TryDisplayTo(Window& win) {
		return true;
	}
};




