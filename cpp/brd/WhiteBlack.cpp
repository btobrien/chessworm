
#include "WhiteBlack.h"
using namespace Chess;


bool White::isThreateningWithPawn(int square) {
	auto pawnSquare = DOWN_RIGHT + square;
	if (IsSquare(pawnSquare) && [pawnSquare] == White::PAWN)
		return true;
	pawnSquare = DOWN_LEFT + square;
	if (IsSquare(pawnSquare) && [pawnSquare] == White::PAWN)
		return true;
	return false;
}

bool Black::isThreateningWithPawn(int square) {
	auto pawnSquare = UP_RIGHT + square;
	if (IsSquare(pawnSquare) && [pawnSquare] == Black::PAWN)
		return true;
	pawnSquare = UP_LEFT + square;
	if (IsSquare(pawnSquare) && [pawnSquare] == Black::PAWN)
		return true;
	return false;
}

void White::SetEnPassant(int fromSquare, int toSquare) {
	if ([toSquare] == White::PAWN && Rank(fromSquare) == '2' && toSquare == (UP_UP + fromSquare)) {
		_enPassant = UP + fromSquare;
		return;
	}
	_enPassant = nullsquare;
}

void Black::SetEnPassant(int fromSquare, int toSquare) {
	if ([toSquare] == Black::PAWN && Rank(fromSquare) == '7' && toSquare == (DOWN_DOWN + fromSquare)) {
		_enPassant = DOWN + fromSquare;
		return;
	}
	_enPassant = nullsquare;
}

void White::SetCastlingRights(int fromSquare, int toSquare) {
    switch (fromSquare) {
        case e1: 
            SetWhiteCastleShort(false);
            _whiteCastleLong = false;
    		break;
        case a1: 
        	_whiteCastleLong = false;
      		break;      
        case h1: 
            _whiteCastleShort = false;
       		break;      
    }
	switch (toSquare) {
        case a8: 
            _blackCastleLong = false;
      		break;      
        case h8: 
            _blackCastleShort = false;
     		break;      
	}
}

void Black::SetCastlingRights(int fromSquare, int toSquare) {
    switch (fromSquare) {
        case e8: 
            _blackCastleShort = false;
            _blackCastleLong = false;
     		break;      
        case a8: 
            _blackCastleLong = false;
      		break;      
        case h8: 
            _blackCastleShort = false;
     		break;      
    }
	switch (toSquare) {
        case a1: 
        	_whiteCastleLong = false;
      		break;      
        case h1: 
            _whiteCastleShort = false;
       		break;      
	}
}

bool White::TryCastleShort() {
	if (!_whiteCastleShort || [f1] || [g1]) 
		return false;
	if (isThreatening<Black>(e1) || isThreatening<Black>(f1) || isThreatening<Black>(g1)) 
		return false;
	[f1] = White::ROOK;
	[g1] = White::KING;
	[h1] = nullpiece;
	[e1] = nullpiece;
	_whiteCastleShort = false;
	_whiteCastleLong = false;
	return true;
}

bool Black::TryCastleShort() {
	if (!_blackCastleShort || [f8] || [g8]) 
		return false;
	if (isThreatening<White>(e8) || isThreatening<White>(f8) || isThreatening<White>(g8)) 
		return false;
	[f8] = Black::ROOK;
	[g8] = Black::KING;
	[h8] = nullpiece;
	[e8] = nullpiece;
	_blackCastleShort = false;
	_blackCastleLong = false;
	return true;
}

bool White::TryCastleLong() {
	if (!_whiteCastleLong || [b1] || [c1] || [d1]) 
		return false;
	if (isThreatening<Black>(e1) || isThreatening<Black>(d1) || isThreatening<Black>(c1)) 
		return false;
	[d1] = White::ROOK;
	[c1] = White::KING;
	[a1] = nullpiece;
	[e1] = nullpiece;
	_whiteCastleShort = false;
	_whiteCastleLong = false;
	return true;
}

bool Black::TryCastleLong() {
	if (!_blackCastleLong || [b8] || [c8] || [d8]) 
		return false;
	if (isThreatening<White>(e8) || isThreatening<White>(d8) || isThreatening<White>(c8)) 
		return false;
	[d8] = Black::ROOK;
	[c8] = Black::KING;
	[a8] = nullpiece;
	[e8] = nullpiece;
	_blackCastleShort = false;
	_blackCastleLong = false;
	return true;
}
