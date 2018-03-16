
#include "BoardState.h"


BoardState::BoardState() : whiteCastleK(true),
						   whiteCastleQ(true),
						   blackCastleK(true),
						   blackCastleQ(true),
						   enPassant(nullsquare),
						   clock(0)
{
	// make static...AND use STATIC CONST VARS!!!!!!!!!!!!!
	std::string init =  "RNBQKBNRPPPPPPPP" + std::string(NUM_SQUARES / 2, 0) +  "pppppppprnbqkbnr";
	memcpy(squares, init.data(), NUM_SQUARES);
}	  


BoardState::BoardState(const BoardState& other) : whiteCastleK(other.whiteCastleQ),
												  whiteCastleQ(other.whiteCastleQ),
												  blackCastleK(other.blackCastleK),
												  blackCastleQ(other.blackCastleQ),
												  enPassant(other.enPassant),
												  clock(other.clock)
{ memcpy(squares, other.squares, NUM_SQUARES); }	  


bool BoardState::IsFileOpen(char fromSquare, char toSquare) {
	int direction = (toSquare - fromSquare) > 0 ? 8 : -8;
	for (int i = fromSquare + direction; i != toSquare; i += direction) {
		if (squares[i])
			return false;
	}
	return true;
}


bool BoardState::IsRankOpen(char fromSquare, char toSquare) {
	int direction = (toSquare - fromSquare) > 0 ? 1 : -1;
	for (int i = fromSquare + direction; i != toSquare; i += direction) {
		if (squares[i])
			return false;
	}
	return true;
}


bool BoardState::IsDiagOpen(char fromSquare, char toSquare) {
	int direction = (toSquare - fromSquare) > 0 ? 8 : -8;
	direction += ((File(toSquare) - File(fromSquare)) > 0 ? 1 : -1);
	for (int i = fromSquare + direction; i != toSquare; i += direction) {
		if (squares[i])
			return false;
	}
	return true;
}


bool BoardState::IsFirstInDirection(int square, Direction direction, char piece0, char piece1) {
	for (int testSquare = direction(square); IsSquare(testSquare); testSquare = direction(testSquare)) {
		if (squares[testSquare])
			return (squares[testSquare] == piece0 || squares[testSquare] == piece1);
	}
	return false;
}


bool BoardState::IsNextInDirection(int square, Direction direction, char piece) {
	int testSquare = direction(square);
	if (IsSquare(testSquare))
		return (squares[testSquare] == piece);
	return false;
}

std::string BoardState::ToString() const {
	bool whiteToMove = (clock % 2 == 0);
	std::string fen;
    int empties = 0;

   //need to account for meaningless en passant target 

    for (int i = 0; i < NUM_SQUARES; i++) {
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
    
    if (whiteToMove)
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


