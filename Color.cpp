
#include "Color.h"


bool White::IsThreatened(int square) {
	int targetSquare = UpRight(square);
	if (IsSquare(targetSquare) && squares[targetSquare] == Black::PAWN)
		return true;
	targetSquare = UpLeft(square);
	if (IsSquare(targetSquare) && squares[targetSquare] == Black::PAWN)
		return true;
	return IsThreatenedHelper<Black::KNIGHT, Black::BISHOP, Black::ROOK, Black::QUEEN, Black::KING>(square);
}

bool Black::IsThreatened(int square) {
	int targetSquare = DownRight(square);
	if (IsSquare(targetSquare) && squares[targetSquare] == White::PAWN)
		return true;
	targetSquare = DownLeft(square);
	if (IsSquare(targetSquare) && squares[targetSquare] == White::PAWN)
		return true;
	return IsThreatenedHelper<White::KNIGHT, White::BISHOP, White::ROOK, White::QUEEN, White::KING>(square);
}


void White::SetEnPassant(int fromSquare, int toSquare) {
	if (squares[toSquare] == PAWN && Rank(fromSquare) == '2' && toSquare == Up(Up(fromSquare))) {
		enPassant = Up(fromSquare);
		return;
	}
	enPassant = nullsquare;
}

void Black::SetEnPassant(int fromSquare, int toSquare) {
	if (squares[toSquare] == PAWN && Rank(fromSquare) == '7' && toSquare == Down(Down(fromSquare))) {
		enPassant = Down(fromSquare);
		return;
	}
	enPassant = nullsquare;
}


void White::SetCastlingRights(int fromSquare, int toSquare) {
    switch (fromSquare) {
        case Square::e1: 
            whiteCastleK = false;
            whiteCastleQ = false;
    		break;
        case Square::a1: 
        	whiteCastleQ = false;
      		break;      
        case Square::h1: 
            whiteCastleK = false;
       		break;      
    }
	switch (toSquare) {
        case Square::a8: 
            blackCastleQ = false;
      		break;      
        case Square::h8: 
            blackCastleK = false;
     		break;      
	}
}

void Black::SetCastlingRights(int fromSquare, int toSquare) {
    switch (fromSquare) {
        case Square::e8: 
            blackCastleK = false;
            blackCastleQ = false;
     		break;      
        case Square::a8: 
            blackCastleQ = false;
      		break;      
        case Square::h8: 
            blackCastleK = false;
     		break;      
    }
	switch (toSquare) {
        case Square::a1: 
        	whiteCastleQ = false;
      		break;      
        case Square::h1: 
            whiteCastleK = false;
       		break;      
	}
}


bool White::TryCastleShort() {
	if (!whiteCastleK ||
		squares[Square::f1] ||
		squares[Square::g1]) return false;
	if (IsThreatened(Square::e1) ||
		IsThreatened(Square::f1) ||
		IsThreatened(Square::g1)) return false;
	squares[Square::f1] = ROOK;
	squares[Square::g1] = KING;
	squares[Square::h1] = nullpiece;
	squares[Square::e1] = nullpiece;
	whiteCastleK = false;
	whiteCastleQ = false;
	return true;
}

bool Black::TryCastleShort() {
	if (!blackCastleK ||
		squares[Square::f8] ||
		squares[Square::g8]) return false;
	if (IsThreatened(Square::e8) ||
		IsThreatened(Square::f8) ||
		IsThreatened(Square::g8)) return false;
	squares[Square::f8] = ROOK;
	squares[Square::g8] = KING;
	squares[Square::h8] = nullpiece;
	squares[Square::e8] = nullpiece;
	blackCastleK = false;
	blackCastleQ = false;
	return true;
}


bool White::TryCastleLong() {
	if (!whiteCastleQ ||
		squares[Square::b1] ||
		squares[Square::c1] ||
		squares[Square::d1]) return false;
	if (IsThreatened(Square::e1) ||
		IsThreatened(Square::d1) ||
		IsThreatened(Square::c1)) return false;
	squares[Square::c1] = KING;
	squares[Square::d1] = ROOK;
	squares[Square::a1] = nullpiece;
	squares[Square::e1] = nullpiece;
	whiteCastleK = false;
	whiteCastleQ = false;
	return true;
}

bool Black::TryCastleLong() {
	if (!blackCastleQ ||
		squares[Square::b8] ||
		squares[Square::c8] ||
		squares[Square::d8]) return false;
	if (IsThreatened(Square::e8) ||
		IsThreatened(Square::d8) ||
		IsThreatened(Square::c8)) return false;
	squares[Square::c8] = KING;
	squares[Square::d8] = ROOK;
	squares[Square::a8] = nullpiece;
	squares[Square::e8] = nullpiece;
	blackCastleK = false;
	blackCastleQ = false;
	return true;
}


void White::CaptureEnPassant() { squares[Down(enPassant)] = nullpiece; }

void Black::CaptureEnPassant() { squares[Up(enPassant)] = nullpiece; }
