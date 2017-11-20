
#include "Board.hpp"

using std::string;

int BoardState::FindOriginalSquare(char piece, int newSquare, const Move& move) {

	int result = -1;

    for (int i = 0; i < 64; i++) {
        if (squares[i] == piece && IsLegalMove(i, newSquare, move)) {
            if (result >= 0)
                return -1;
            result = i;
        }
    }
	return result;
}

void BoardState::SetEnPassantSquareWhite(int originalSquare, int newSquare, const Move& move) {

	if (move.piece != 'P') {
		enPassant = -1;
		return;
	}

	if (originalSquare >= Square::a2 &&
		originalSquare <= Square::h2 &&
		originalSquare == newSquare - 16)
	{
		enPassant = originalSquare + 8;
	}
}

void BoardState::SetEnPassantSquareBlack(int originalSquare, int newSquare, const Move& move) {

	if (move.piece != 'P') {
		enPassant = -1;
		return;
	}

	if (originalSquare >= Square::a7 &&
		originalSquare <= Square::h7 &&
		originalSquare == NewSquare(move) + 16)
	{
		enPassant = originalSquare - 8;
	}
}

void BoardState::SetCastleRightsWhite(int originalSquare, int newSquare) {
	
    switch (originalSquare) {

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

	switch (newSquare) {

        case Square::a8: 
            blackCastleQ = false;
      		break;      
 
        case Square::h8: 
            blackCastleK = false;
     		break;      
	}
}

void BoardState::SetCastleRightsBlack(int originalSquare, int newSquare) {

    switch (originalSquare) {
		
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

	switch (newSquare) {
    
        case Square::a1: 
        	whiteCastleQ = false;
      		break;      
        
        case Square::h1: 
            whiteCastleK = false;
       		break;      
	}
}

bool BoardState::BlockTest(int sq1, int sq2) {
    return true;
}

bool BoardState::IsThreatened(int sq) {
	return false;
}

bool BoardState::IsValid() {
	clock++;
	return true; 
}

bool BoardState::TryMoveBlack(const Move& move) {

	int newSquare = NewSquare(move);
    
	if (move.castleShort) {
        
        if (!blackCastleK ||
            squares[Square::f8] ||
            squares[Square::g8]) return false;
        
		if (IsThreatened(Square::e8) ||
			IsThreatened(Square::f8) ||
			IsThreatened(Square::g8)) return false;

        squares[Square::f8] = 'k';
        squares[Square::g8] = 'r';
        squares[Square::h8] = 0;
		blackCastleK = false;
		blackCastleQ = false;
		return IsValid();
    }

	else if (move.castleLong) {
        
        if (!blackCastleQ ||
            squares[Square::b8] ||
            squares[Square::c8] ||
            squares[Square::d8]) return false;
        
		if (IsThreatened(Square::e8) ||
			IsThreatened(Square::d8) ||
			IsThreatened(Square::c8)) return false;

        squares[Square::c8] = 'k';
        squares[Square::d8] = 'r';
        squares[Square::a8] = 0;
		blackCastleK = false;
		blackCastleQ = false;
		return IsValid();
   }
	
    else if (move.piece == 'P' && enPassant == newSquare) {
        if (!move.takes)
            return false;
        squares[enPassant + 8] = 0;
    }

	else if (squares[newSquare] != move.takes)
		return false;
	

	char piece = move.piece - ('A' - 'a');

    int originalSquare = FindOriginalSquare(piece, newSquare, move);
    
    if (originalSquare < 0)
        return false;
    
    squares[originalSquare] = 0;
    squares[newSquare] = piece;

	SetEnPassantSquareBlack(originalSquare, newSquare, move);
	SetCastleRightsBlack(originalSquare, newSquare);
    
    return IsValid();
}

bool BoardState::TryMoveWhite(const Move& move) {

	int newSquare = NewSquare(move);
    
	if (move.castleShort) {
        
        if (!whiteCastleK ||
            squares[Square::f1] ||
            squares[Square::g1]) return false;
        
		if (IsThreatened(Square::e1) ||
			IsThreatened(Square::f1) ||
			IsThreatened(Square::g1)) return false;

        squares[Square::f1] = 'K';
        squares[Square::g1] = 'R';
        squares[Square::h1] = 0;
		whiteCastleK = false;
		whiteCastleQ = false;
		return IsValid();
    }

	else if (move.castleLong) {
        
        if (!whiteCastleQ ||
            squares[Square::b1] ||
            squares[Square::c1] ||
            squares[Square::d1]) return false;
        
		if (IsThreatened(Square::e1) ||
			IsThreatened(Square::d1) ||
			IsThreatened(Square::c1)) return false;

        squares[Square::c8] = 'K';
        squares[Square::d8] = 'R';
        squares[Square::a8] = 0;
		whiteCastleK = false;
		whiteCastleQ = false;
		return IsValid();
   }
	
    else if (move.piece == 'P' && enPassant == newSquare) {
        if (!move.takes)
            return false;
        squares[enPassant - 8] = 0;
    }

	else if (squares[newSquare] != move.takes)
		return false;
	

    int originalSquare = FindOriginalSquare(move.piece, newSquare, move);
    
    if (originalSquare < 0)
        return false;
    
    squares[originalSquare] = 0;
    squares[newSquare] = move.piece;

	SetEnPassantSquareWhite(originalSquare, newSquare, move);
	SetCastleRightsWhite(originalSquare, newSquare);
    
    return IsValid();
}

bool BoardState::IsLegalMove(int originalSquare, int newSquare, const Move& move) {
	
    char fromFile = 'a' + (originalSquare % 8);
    int fromRank = 1 + (originalSquare / 8);
    
    if (move.fromFile && fromFile != move.fromFile)
        return false;
    if (move.fromRank && fromRank != move.fromRank)
        return false;
    if (!BlockTest(originalSquare, newSquare))
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
            if (move.takes && (manhattanDifference || manhattanDistance != 2))
                return false;
            if (!move.takes && fileDistance)
                return false;
            if (rankDistance > 2)
                return false;
            if (fromRank != 2 && fromRank != 7 && rankDistance > 1)
                return false;
            break;
            
        case 'N':
			if (manhattanDistance != 3 || manhattanDifference != 1) 
                return false;
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
    return true;
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


