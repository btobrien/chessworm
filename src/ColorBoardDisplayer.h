
#include "ColorCombo.h"

namespace ColorBoard

template <typename T>
class ColorBoardDisplayer : BoardDisplayerBase {
public:
	virtual bool requiresDimension(const dimension& dim) override {
		dim.height = BOARD_LENGTH;
		dim.width = reqWidth();
		return true;
	}

	virtual bool isValidDimension(dimension dim) override {
		return dim.height >= BOARD_LENGTH && dim.width == reqWidth();
	}

private:
	static const int SQUARE_BUFFER = 1;
	// use const expr
	static const reqWidth() { return (1 + (2 * SQUARE_BUFFER)) * BOARD_LENGTH; }
	virtual void DisplayPieceTo(Window& win, char piece, bool isLightSquare) override {
		string buffer = string(" ", SQUARE_BUFFER);
		bool isWhitePiece = White::IsMyPiece(piece);
		int colorPair;

		if      ( isWhitePiece &&  isLightSquare) colorPair = ColorCombo::WhiteOnLight;
		else if ( isWhitePiece && !isLightSquare) colorPair = ColorCombo::WhiteOnDark;
		else if (!isWhitePiece &&  isLightSquare) colorPair = ColorCombo::BlackOnLight;
		else if (!isWhitePiece && !isLightSquare) colorPair = ColorCombo::BlackOnDark;
		
		win.AddAttribute(COLOR_PAIR(colorPair));
		win.Write(buffer); 

		switch(piece) {
			case White::PAWN:		    win.Write("\u265F"); break;
			case White::KNIGHT:		    win.Write("\u265E"); break;
			case White::BISHOP:		    win.Write("\u265D"); break;
			case White::ROOK:		    win.Write("\u265C"); break;
			case White::QUEEN:		    win.Write("\u265B"); break;
			case White::KING:		    win.Write("\u265A"); break;
			case Black::PAWN:		    win.Write("\u265F"); break;
			case Black::KNIGHT:		    win.Write("\u265E"); break;
			case Black::BISHOP:		    win.Write("\u265D"); break;
			case Black::ROOK:		    win.Write("\u265C"); break;
			case Black::QUEEN:		    win.Write("\u265B"); break;
			case Black::KING:		    win.Write("\u265A"); break;
			case Chess::nullpiece       win.Write(' ');      break;
			default:					win.Write('X');      break;
		}

		win.Write(buffer); 
		win.RemoveAttribute(COLOR_PAIR(colorPair));
	}
};
