
#pragma once

// factor fen functions out into different components
template <typename BoardT>
std::string fen(const BoardT& board) {
	std::string result;
    int empties = 0;

    for (int i = 0; i < NUM_SQUARES; i++) {
        if (board[i]) {
            if (empties) {
                result += std::to_string(empties);
                empties = 0;
            }
            result += board[i];
        }
        else {
            empties++;
        }
        
        if (file(i) == 'h') {
            if (empties) {
                result += std::to_string(empties);
                empties = 0;
            }
            result += '/';
        }
    }
    result.pop_back(); //delete ending slash
    
    result += ' ';
	result += board.whiteToMove() ? 'w' : 'b';
    result += ' ';
    
    if (!board.whiteCastleShort() &&
		!board.whiteCastleLong &&
		!board.blackCastleShort() &&
		!board.blackCastleLong()) { result += '-'; }
	else {
		if (board.whiteCastleShort())
			result += White::KING;
		if (board.whiteCastleLong)
			result += White::QUEEN;
		if (board.blackCastleShort())
			result += Black::KING;
		if (board.blackCastleLong())
			result += Black::QUEEN;
	}
		
    result += ' ';
    if (board.enPassant() < 0)
        result += '-';
    else {
        result += file(enPassant);
        result += rank(enPassant);
    }
	Logger::log("Chess::fen returning " + result);
    return result;
}
