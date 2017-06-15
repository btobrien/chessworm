//
//  main.cpp
//  ChessWorm
//
//  Created by Bret O'Brien on 1/4/17.
//  Copyright © 2017 Bret O'Brien. All rights reserved.
//

#include <iostream>
#include <vector>
#include "ChessWorm.hpp"
#include "Parse.hpp"
#include <fstream>
using namespace std;



int main(int argc, const char * argv[]) {
    
      //have file names passed in as command line arguments
    
    ChessWorm chessWorm;
    
    ifstream pgn;
    
    pgn.open("test_2.txt");
    
    if(pgn.is_open()) {
        
        while(!pgn.eof()) {
            chessWorm.games.push_back(new Game(pgn));
        }
    }
    else {
        cout << "Test file failed to open" << endl;
    }
    
    pgn.close();
    
    chessWorm.study();
    
}
