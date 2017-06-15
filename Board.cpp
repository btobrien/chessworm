//
//  Board.cpp
//  ChessWorm
//
//  Created by Bret O'Brien on 6/3/17.
//  Copyright © 2017 Bret O'Brien. All rights reserved.
//

#include "Board.hpp"

/*-----------------------------------------------------------------------*/
/*----------------------------  Board  ----------------------------------*/
/*-----------------------------------------------------------------------*/



Board::Board() : whiteCastleK(true), whiteCastleQ(true), blackCastleK(true), blackCastleQ(true), enPassantTarget(-1), moveClock(0) {
    
    string white = "RNBQKBNRPPPPPPPP";
    string black = "pppppppprnbqkbnr";
    
    for (int i = 0; i < 16; i++) {
        squares[i] = white[i];
        squares[63 - i] = black[15 - i];
    }
    for (int i = 0; i < 32; i++) {
        squares[i] = 0;
    }
}

bool Board::whiteToMove() {
    return moveClock % 2 == 0;
}

vector<int> Board::getSquares(char p) {
    vector<int> squares;
    return squares;
}

bool Board::passCheckTest(int sq1, int sq2) {
    return true;
}
bool Board::passBlockTest(int sq1, int sq2) {
    return true;
}
bool Board::passFriendlyFireTest(int sq1, int sq2) {
    return true;
}

//change all asserts to return falses eventually

bool Board::update(Move m) {
    
    char piece = m.piece();
    
    if (!whiteToMove()) {
        piece -= ('A' - 'a');
    }
    
    //Edge Cases regarding Auxilary squares
    
    if (whiteToMove() && m.castlesShort()) {
        
        assert(piece == 'K');
        assert(!m.castlesLong());
        assert(whiteCastleK);
        assert(squares[Square::e1] == 'K');
        assert(!squares[Square::f1]);
        assert(!squares[Square::g1]);
        assert(squares[Square::h1] == 'R');
        
        squares[Square::f1] = 'R';
        squares[Square::h1] = 0;
        
    }
    else if (whiteToMove() && m.castlesLong()) {
        
        assert(piece == 'K');
        assert(!m.castlesShort());
        assert(whiteCastleQ);
        assert(squares[Square::e1] == 'K');
        assert(!squares[Square::d1]);
        assert(!squares[Square::c1]);
        assert(!squares[Square::b1]);
        assert(squares[Square::a1] == 'R');
        
        squares[Square::d1] = 'R';
        squares[Square::a1] = 0;
        
    }
    else if (!whiteToMove() && m.castlesShort()) {
        
        assert(piece == 'k');
        assert(!m.castlesLong());
        assert(blackCastleK);
        assert(squares[Square::e8] == 'k');
        assert(!squares[Square::f8]);
        assert(!squares[Square::g8]);
        assert(squares[Square::h8] == 'r');
        
        squares[Square::f8] = 'r';
        squares[Square::h8] = 0;
        
    }
    else if (!whiteToMove() && m.castlesLong()) {
        
        assert(piece == 'k');
        assert(blackCastleQ);
        assert(!m.castlesShort());
        assert(squares[Square::e8] == 'k');
        assert(!squares[Square::d8]);
        assert(!squares[Square::c8]);
        assert(!squares[Square::b8]);
        assert(squares[Square::a8] == 'r');
        
        squares[Square::d8] = 'r';
        squares[Square::a8] = 0;
    }
    else if (enPassantTarget == m.newSquare() && m.takes()) {
        
        assert(m.piece() == 'P');
        
        if (!whiteToMove()) {
            assert(squares[enPassantTarget + 8] == 'P');
            assert(enPassantTarget >= Square::a2 && enPassantTarget <= Square::h2);
            squares[enPassantTarget + 8] = 0;
        }
        else {
            assert(squares[enPassantTarget - 8] == 'p');
            assert(enPassantTarget >= Square::a7 && enPassantTarget <= Square::h7);
            squares[enPassantTarget - 8] = 0;
        }
    }
    else if (m.takes() && whiteToMove()) {
        assert((squares[m.newSquare()] >= 'b' && squares[m.newSquare()] <= 'q'));
    }
    else if (m.takes() && !whiteToMove()) {
        assert(squares[m.newSquare()] >= 'B' && squares[m.newSquare()] <= 'Q');
    }
    else {
        assert(!squares[m.newSquare()]);
    }
    
    //Competition! Find the orginal square of the moving piece

    vector<int> candidates;
    candidates = getSquares(piece);
    int winner = -1;
    
    for (int i = 0; i < candidates.size(); i++) {
        if (legalMove(m, candidates[i])) {
            assert(winner == -1);
            winner = candidates[i];
        }
    }
    assert(winner != -1);
    
    squares[winner] = 0;
    squares[m.newSquare()] = piece;
    
    
    //Update Non Board State regarding the winning square
    
    enPassantTarget = -1;
    
    if (m.piece() == 'P') {
        if (whiteToMove()) {
            if ((winner >= Square::a2 && winner <= Square::h2) && (winner == m.newSquare() - 16)) {
                enPassantTarget = winner + 8;
            }
        }
        else {
            if ((winner >= Square::a7 && winner <= Square::h7) && (winner == m.newSquare() + 16)) {
                enPassantTarget = winner - 8;
            }
        }
    }
    
    switch(winner) {
        case Square::e1: {
            whiteCastleK = false;
            whiteCastleQ = false;
        }
        case Square::e8: {
            blackCastleK = false;
            blackCastleQ = false;
        }
        case Square::a1: {
            whiteCastleQ = false;
        }
        case Square::h1: {
            whiteCastleK = false;
        }
        case Square::a8: {
            blackCastleQ = false;
        }
        case Square::h8: {
            blackCastleK = false;
        }
    }
    
    moveClock++;
    
    return true;
}

bool Board::legalMove(Move m, int sq) {
    
    char sqFile = 'a' + (sq % 8);
    int sqRank = 1 + sq / 8;

    
    if (m.oldFile() && sqFile != m.oldFile()) {
        return false;
    }
    if (m.oldRank() && sqRank != m.oldRank()) {
        return false;
    }
    if (!passCheckTest(sq, m.newSquare())) {
        return false;
    }
    if (!passFriendlyFireTest(sq, m.newSquare())) {
        return false;
    }
    if (m.piece() != 'N' && !passBlockTest(sq, m.newSquare())) {
        return false;
    }
    
    switch(m.piece()) {
            
        case 'P': {
            
            if (!whiteToMove() && (m.rank() - sqRank) >= 0) {
                return false;
            }
            if (whiteToMove() && (m.rank() - sqRank) <= 0) {
                return false;
            }
            if (m.takes() && (abs(sqFile - m.file()) != 1)) {
                return false;
            }
            if (m.takes() && (abs(sqRank - m.rank()) != 1)) {
                return false;
            }
            if (!m.takes() && sqFile != m.file()) {
                return false;
            }
            if (abs(sqRank - m.rank()) > 2) {
                return false;
            }
            if (sqRank != 2 && sqRank != 7 && (abs(sqRank - m.rank()) > 1)) {
                return false;
            }
        }
            
        case 'N': {
            
            int manhattanDistance = abs(sqRank - m.rank()) + abs(sqFile - m.file());
            
            if (manhattanDistance != 3) {
                return false;
            }
        }
                case 'B': {
                    
                }
                case 'R': {
                    
                }
                case 'Q': {
                    
                }
                case 'K': {
                    
                }
                default: {
                    return false;
                }
                }
                }


string Board::getFen(bool clocks = false) {
    string fen;
    int empties = 0;
    
    for (int i = 0; i < 64; i++) {
        
        if (squares[i]) {
            if (empties) {
                fen += to_string(empties);
                empties = 0;
            }
            fen += squares[i];
        }
        else {
            empties++;
        }
        
        if (i % 8 == 7) {
            
            if (empties) {
                fen += to_string(empties);
                empties = 0;
            }
            fen += '/';
        }
    }
    
    fen.pop_back(); //delete ending slash
    
    
    fen += ' ';
    
    if (whiteToMove())
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
    
    if (enPassantTarget == -1)
        fen += '-';
    else {
        fen += ('a' + enPassantTarget / 8);
        fen += (enPassantTarget % 8);
    }
    
    if (clocks) {
        fen += " 0 ";
        fen += to_string(moveClock);
    }
    
    return fen;
}



