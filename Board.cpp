
#include "Board.h"

using std::string;
using std::cerr;
using std::endl;



int BoardState::TryFindAndMovePiece(char piece, int toSquare, Move move) {

	int result = -1;

    for (int i = 0; i < 64; i++) {
        if (squares[i] == piece && IsLegalMove(i, toSquare, move)) {
            if (result >= 0)
                return -1;
            result = i;
        }
    }
	
	squares[result] = 0;
	squares[toSquare] = piece;

	return result;
}

void BoardState::SetEnPassantSquareWhite(int fromSquare, int toSquare, Move move) {

	if (move.piece != 'P') {
		enPassant = -1;
		return;
	}

	if (fromSquare >= Square::a2 &&
		fromSquare <= Square::h2 &&
		fromSquare == toSquare - 16)
	{
		enPassant = fromSquare + 8;
	}
}

void BoardState::SetEnPassantSquareBlack(int fromSquare, int toSquare, Move move) {

	if (move.piece != 'P') {
		enPassant = -1;
		return;
	}

	if (fromSquare >= Square::a7 &&
		fromSquare <= Square::h7 &&
		fromSquare == Square(move) + 16)
	{
		enPassant = fromSquare - 8;
	}
}

void BoardState::SetCastleRightsWhite(int fromSquare, int toSquare) {
	
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

void BoardState::SetCastleRightsBlack(int fromSquare, int toSquare) {

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


bool BoardState::TryMoveBlack(Move move) {

	int toSquare = Square(move);
    
	if (move.castleShort) {
        
        if (!blackCastleK ||
            squares[Square::f8] ||
            squares[Square::g8]) return false;
        
		if (IsThreatenedByWhite(Square::e8) ||
			IsThreatenedByWhite(Square::f8) ||
			IsThreatenedByWhite(Square::g8)) return false;

        squares[Square::f8] = 'r';
        squares[Square::g8] = 'k';
        squares[Square::h8] = 0;
        squares[Square::e8] = 0;
		blackCastleK = false;
		blackCastleQ = false;
		return true;
    }

	if (move.castleLong) {
        
        if (!blackCastleQ ||
            squares[Square::b8] ||
            squares[Square::c8] ||
            squares[Square::d8]) return false;
        
		if (IsThreatenedByWhite(Square::e8) ||
			IsThreatenedByWhite(Square::d8) ||
			IsThreatenedByWhite(Square::c8)) return false;

        squares[Square::c8] = 'k';
        squares[Square::d8] = 'r';
        squares[Square::a8] = 0;
        squares[Square::e8] = 0;
		blackCastleK = false;
		blackCastleQ = false;
		return true;
   }
	
	if (IsBlackPiece(squares[toSquare]))
		return false;
    if (move.piece == 'P' && enPassant == toSquare) {
		move.takes = true;
        squares[enPassant + 8] = 0;
    }
	else {
		if (move.takes && !squares[toSquare])
			return false;
		if (IsWhitePiece(squares[toSquare]))
			move.takes = true;
	}

	char piece = move.piece - ('A' - 'a');

    int fromSquare = TryFindAndMovePiece(piece, toSquare, move);
    
    if (fromSquare < 0)
        return false;
    
	SetEnPassantSquareBlack(fromSquare, toSquare, move);
	SetCastleRightsBlack(fromSquare, toSquare);
    
    return true;
}

bool BoardState::TryMoveWhite(Move move) {

	cerr << "TryMoveWhite: Entered" << endl; 

	int toSquare = Square(move);
    
	if (move.castleShort) {
        
        if (!whiteCastleK ||
            squares[Square::f1] ||
            squares[Square::g1]) return false;
        
		if (IsThreatenedByBlack(Square::e1) ||
			IsThreatenedByBlack(Square::f1) ||
			IsThreatenedByBlack(Square::g1)) return false;

        squares[Square::f1] = 'R';
        squares[Square::g1] = 'K';
        squares[Square::h1] = 0;
        squares[Square::e1] = 0;
		whiteCastleK = false;
		whiteCastleQ = false;
		return true;
    }

	if (move.castleLong) {
        
        if (!whiteCastleQ ||
            squares[Square::b1] ||
            squares[Square::c1] ||
            squares[Square::d1]) return false;
        
		if (IsThreatenedByBlack(Square::e1) ||
			IsThreatenedByBlack(Square::d1) ||
			IsThreatenedByBlack(Square::c1)) return false;

        squares[Square::c1] = 'K';
        squares[Square::d1] = 'R';
        squares[Square::a1] = 0;
        squares[Square::e1] = 0;
		whiteCastleK = false;
		whiteCastleQ = false;
		return true;
	}
	
	if (IsWhitePiece(squares[toSquare]))
		return false;
    if (move.piece == 'P' && enPassant == toSquare) {
		move.takes = true;
        squares[enPassant - 8] = 0;
    }
	else {
		if (move.takes && !squares[toSquare])
			return false;
		if (IsBlackPiece(squares[toSquare]))
			move.takes = true;
	}

    int fromSquare = TryFindAndMovePiece(move.piece, toSquare, move);
    
    if (fromSquare < 0)
        return false;

	SetEnPassantSquareWhite(fromSquare, toSquare, move);
	SetCastleRightsWhite(fromSquare, toSquare);
    
    return true;
}

bool BoardState::IsLegalMove(int fromSquare, int toSquare, Move move) {
	
	if (fromSquare == toSquare)
		return false;

    char fromFile = File(fromSquare);
    char fromRank = Rank(fromSquare);
    
    if (move.fromFile && fromFile != move.fromFile)
        return false;
    if (move.fromRank && fromRank != move.fromRank)
        return false;
    
	int fileDistance = abs(fromFile - move.toFile);
  	int rankDistance = abs(fromRank - move.toRank);
	int manhattanDistance = fileDistance + rankDistance;
	int manhattanDifference = abs(fileDistance - rankDistance); 
	bool isMovingUp = move.toRank - fromRank > 0;

	if (fileDistance && !rankDistance && !RankOpen(fromSquare, toSquare)) 
		return false;
	if (!fileDistance && rankDistance && !IsFileOpen(fromSquare, toSquare)) 
		return false;
	if (!manhattanDifference && !IsDiagOpen(fromSquare, toSquare)) 
		return false;

	switch (move.piece) {
        case 'P':
			if (!rankDistance)
				return false;
            if (isMovingUp != WhiteToMove())
                return false;
            if (move.takes) {
				if (manhattanDifference || manhattanDistance != 2)
					return false;
				break;
			}
			if (fileDistance)
				return false;
			if (rankDistance > 2)
				return false;
			if (fromRank != '2' && fromRank != '7' && rankDistance > 1)
				return false;
			break;
        case 'N':
			if (manhattanDistance != 3 || manhattanDifference != 1)
				return false;
			break;
		case 'B': 
			if (manhattanDifference)
				return false;
			break;
   		case 'R':
			if (rankDistance && fileDistance)
				return false;
			break;
   		case 'Q':
			if (manhattanDifference && rankDistance && fileDistance)
				return false;
			break;
   		case 'K':
			if (manhattanDifference && manhattanDistance != 1)
				return false;
			if (!manhattanDifference && manhattanDistance != 2)
				return false;
			break;
		default:
			return false;
	}

	return CheckTest(fromSquare, toSquare);
}

string BoardState::ToString() const {
    string fen;
    int empties = 0;

   //need to account for meaningless en passant target 

    for (int i = 0; i < 64; i++) {
        if (squares[i]) {
            if (empties) {
                fen += std::to_string(empties);
                empties = 0;
            }
            fen += squares[i];
        }
        else {
            empties++;
        }
        
        if (i % 8 == 7) {
            if (empties) {
                fen += std::to_string(empties);
                empties = 0;
            }
            fen += '/';
        }
    }
    
    fen.pop_back(); //delete ending slash
    
    fen += ' ';
    
    if (WhiteToMove())
        fen += 'w';
    else
        fen += 'b';
    
    fen += ' ';
    
    if (!whiteCastleQ && !whiteCastleK && !blackCastleK && !blackCastleQ)
        fen += '-';
    else if (whiteCastleK)
        fen += 'K';
    else if (whiteCastleQ)
        fen += 'K';
    else if (blackCastleK)
        fen += 'k';
    else if (blackCastleQ)
        fen += 'q';
    
    fen += ' ';
    
    if (enPassant == -1)
        fen += '-';
    else {
        fen += ('a' + enPassant / 8);
        fen += (enPassant % 8);
    }
    
    return fen;
}


