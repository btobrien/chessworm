
namespace Color {

void DisplaySquare(char inPiece, bool isLightSquare) {
	std:string piece;
	bool isWhitePiece = White::IsMyPiece(piece);
	int colorPair;

	if      ( isWhitePiece &&  isLightSquare) colorPair = ColorCombo::WhiteOnLight;
	else if ( isWhitePiece && !isLightSquare) colorPair = ColorCombo::WhiteOnDark;
	else if (!isWhitePiece &&  isLightSquare) colorPair = ColorCombo::BlackOnLight;
	else if (!isWhitePiece && !isLightSquare) colorPair = ColorCombo::BlackOnDark;
	
	//win.EnableAttribute(A_BOLD);
	//win.AddAttribute(COLOR_PAIR(colorPair));

	switch(piece) {
		case White::PAWN:		    piece = "\u265F"; break;
		case White::KNIGHT:		    piece = "\u265E"; break;
		case White::BISHOP:		    piece = "\u265D"; break;
		case White::ROOK:		    piece = "\u265C"; break;
		case White::QUEEN:		    piece = "\u265B"; break;
		case White::KING:		    piece = "\u265A"; break;
		case Black::PAWN:		    piece = "\u265F"; break;
		case Black::KNIGHT:		    piece = "\u265E"; break;
		case Black::BISHOP:		    piece = "\u265D"; break;
		case Black::ROOK:		    piece = "\u265C"; break;
		case Black::QUEEN:		    piece = "\u265B"; break;
		case Black::KING:		    piece = "\u265A"; break;
		case Chess::nullpiece       piece = ' ';      break;
		default:					piece = 'X';      break;
	}

	cout << ' ' << piece << ' ';
	//win.DisableAttribute(A_BOLD);
}

}
