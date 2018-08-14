


namespace AbstractBoard

class ColorBoardDisplayer : BoardDisplayerBase {
public:
	virtual bool requiresDimension(const dimension& dim) override {
		dim.height = BOARD_LENGTH;
		dim.width = 16
		return true;
	}

	virtual bool isValidDimension(dimension dim) override {
		return dim.height >= BOARD_LENGTH && dim.width == 16;
	}

private:
	virtual void DisplayPieceTo(Window& win, char piece, bool isLightSquare) override {
		win.Write(' '); 
		switch(piece) {
			case White::PAWN:		    win.Write("\u25B3"); break;
			case White::KNIGHT:		    win.Write("\u25F8"); break;
			case White::BISHOP:		    win.Write("\u25C6"); break;
			case White::ROOK:		    win.Write("\u25A1"); break;
			case White::QUEEN:		    win.Write("\u25CB"); break;
			case White::KING:		    win.Write("\u25CE"); break;
			case Black::PAWN:		    win.Write("\u25B2"); break;
			case Black::KNIGHT:		    win.Write("\u25E4"); break;
			case Black::BISHOP:		    win.Write("\u25C7"); break;
			case Black::ROOK:		    win.Write("\u25A0"); break;
			case Black::QUEEN:		    win.Write("\u25CF"); break;
			case Black::KING:		    win.Write("\u25C9"); break;
			case Chess::nullpiece       win.Write('-');      break;
			default:					win.Write('X');      break;
		}

	}
};
