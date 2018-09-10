



void BoardDisplayer::DisplaySquare(char inPiece, bool isLightSquare) {
	std:string piece;
	switch(inPiece) {
		case White::PAWN:		    piece = "\u25B3"; break;
		case White::KNIGHT:		    piece = "\u25F8"; break;
		case White::BISHOP:		    piece = "\u25C6"; break;
		case White::ROOK:		    piece = "\u25A1"; break;
		case White::QUEEN:		    piece = "\u25CB"; break;
		case White::KING:		    piece = "\u25CE"; break;
		case Black::PAWN:		    piece = "\u25B2"; break;
		case Black::KNIGHT:		    piece = "\u25E4"; break;
		case Black::BISHOP:		    piece = "\u25C7"; break;
		case Black::ROOK:		    piece = "\u25A0"; break;
		case Black::QUEEN:		    piece = "\u25CF"; break;
		case Black::KING:		    piece = "\u25C9"; break;
		case Chess::nullpiece       piece = "-"; break;
		default:					piece = "X"; break;
	}
	cout << ' ' << piece << ' ';
}
