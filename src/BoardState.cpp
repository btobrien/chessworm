
#include "BoardState.h"
#include "Color.h"

using std::string;


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
        
        if (File(i) == 'h') {
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
    if (whiteCastleK)
        fen += White::KING;
    if (whiteCastleQ)
        fen += White::QUEEN;
    if (blackCastleK)
        fen += Black::KING;
    if (blackCastleQ)
        fen += Black::QUEEN;
    
    fen += ' ';
    
    if (enPassant == -1)
        fen += '-';
    else {
        fen += File(enPassant);
        fen += Rank(enPassant);
    }
	Logger::log("BoardState::ToString returning " + fen);
    return fen;
}


