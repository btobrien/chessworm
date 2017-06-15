//
//  Board.hpp
//  ChessWorm
//
//  Created by Bret O'Brien on 6/3/17.
//  Copyright © 2017 Bret O'Brien. All rights reserved.
//

#ifndef Board_hpp
#define Board_hpp

#include <stdio.h>
#include "Parse.hpp"

class Board {
    
private:
    
    char squares[64];
    
    int moveClock;
    
    int enPassantTarget;
    
    bool whiteCastleK;
    bool whiteCastleQ;
    bool blackCastleK;
    bool blackCastleQ;
    
    bool passCheckTest(int sq1, int sq2);
    bool passBlockTest(int sq1, int sq2);
    bool passFriendlyFireTest(int sq1, int sq2);
    
    bool legalMove(Move m, int sq);
    
public:
    
    Board();

    char getPiece(int sq); //overload this?
    vector<int> getSquares(char p);
    
    bool update(Move m); //returns false if illegal move
    string getFen(bool clocks);
    
    bool whiteToMove();

    bool getWhiteCastleK();
    bool getWhiteCastleQ();
    bool getBlackCastleK();
    bool getBlackCastleQ();
    int getMoveClock();
    
    enum Square {a1, b1, c1, d1, e1, f1, g1, h1, a2, b2, c2, d2, e2, f2, g2, h2, a3, b3, c3, d3, e3, f3, g3, h3, a4, b4, c4, d4, e4, f4, g4, h4, a5, b5, c5, d5, e5, f5, g5, h5, a6, b6, c6, d6, e6, f6, g6, h6, a7, b7, c7, d7, e7, f7, g7, h7, a8, b8, c8, d8, e8, f8, g8, h8 };
    
};


#endif /* Board_hpp */
