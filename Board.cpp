
#include "Board.h"

using std::string;
using std::cerr;
using std::endl;

int BoardState::FindOriginalSquare(char piece, int toSquare, Move move) {

	int result = -1;

    for (int i = 0; i < 64; i++) {
		if (i == Square::e5)
			cerr << "Piece should be here"; 
        if (squares[i] == piece && IsLegalMove(i, toSquare, move)) {
            if (result >= 0)
                return -1;
            result = i;
        }
    }
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
		fromSquare == NewSquare(move) + 16)
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

bool BoardState::BlockTest(int fromSquare, int toSquare) {
	if (fromSquare == toSquare)
		return false;
	if (squares[fromSquare] == 'N' || squares[fromSquare] == 'n')
		return true;
	
    return true;
}

bool BoardState::IsThreatenedByBlack(int sq) {
	return false;
}

bool BoardState::IsThreatenedByWhite(int sq) {
	return false;
}

bool BoardState::IsValid() {
	clock++;
	return true; 
}

bool BoardState::TryMoveBlack(Move move) {

	cerr << "TryMoveBlack: Entered\n";

	int toSquare = NewSquare(move);
    
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
		return IsValid();
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
		return IsValid();
   }
	
    if (move.piece == 'P' && enPassant == toSquare) {
        squares[enPassant + 8] = 0;
		move.takes = true;
    }
	else {
		move.takes = IsWhitePiece(squares[toSquare]);
	}

	char piece = move.piece - ('A' - 'a');

    int fromSquare = FindOriginalSquare(piece, toSquare, move);
    
    if (fromSquare < 0)
        return false;
    
    squares[fromSquare] = 0;
    squares[toSquare] = piece;

	SetEnPassantSquareBlack(fromSquare, toSquare, move);
	SetCastleRightsBlack(fromSquare, toSquare);
    
    return IsValid();
}

bool BoardState::TryMoveWhite(Move move) {

	cerr << "TryMoveWhite: Entered" << endl; 

	int toSquare = NewSquare(move);
    
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
		return IsValid();
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
		return IsValid();
   }
	
    if (move.piece == 'P' && enPassant == toSquare) {
		move.takes = true;
        squares[enPassant - 8] = 0;
    }
	else {
		move.takes = IsBlackPiece(squares[toSquare]);
	}


    int fromSquare = FindOriginalSquare(move.piece, toSquare, move);
    
    if (fromSquare < 0)
        return false;
    
    squares[fromSquare] = 0;
    squares[toSquare] = move.piece;

	SetEnPassantSquareWhite(fromSquare, toSquare, move);
	SetCastleRightsWhite(fromSquare, toSquare);
    
    return IsValid();
}

bool BoardState::IsLegalMove(int fromSquare, int toSquare, Move move) {
	
    char fromFile = 'a' + (fromSquare % 8);
    char fromRank = '1' + (fromSquare / 8);
    
    if (move.fromFile && fromFile != move.fromFile)
        return false;
    if (move.fromRank && fromRank != move.fromRank)
        return false;
    if (!BlockTest(fromSquare, toSquare))
        return false;
    
	int fileDistance = abs(fromFile - move.toFile);
  	int rankDistance = abs(fromRank - move.toRank);
	int manhattanDistance = fileDistance + rankDistance;
	int manhattanDifference = abs(fileDistance - rankDistance); 
	bool isMovingUp = move.toRank - fromRank > 0;

	switch (move.piece) {
        case 'P':
			if (!rankDistance)
				return false;
            if (isMovingUp != WhiteToMove())
                return false;
            if (move.takes) {
				if (manhattanDifference || manhattanDistance != 2)
	                return false;
			}
			else {
				if (fileDistance)
					return false;
				if (rankDistance > 2)
					return false;
				if (fromRank != '2' && fromRank != '7' && rankDistance > 1)
					return false;
			}
            return true;
        case 'N': return (manhattanDistance == 3 && manhattanDifference == 1);
		case 'B': return (!manhattanDifference);
   		case 'R': return (!rankDistance || !fileDistance);
   		case 'Q': return (!manhattanDifference || !rankDistance || !fileDistance);
   		case 'K':
			if (manhattanDifference && manhattanDistance != 1)
				return false;
			if (!manhattanDifference && manhattanDistance != 2)
				return false;
			return true;
		default:
			return false;
	}
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


