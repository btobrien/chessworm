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

#include <locale.h>

#include <ncurses.h>

using namespace std;



int main(int argc, const char * argv[]) {
    
    setlocale(LC_ALL, "");
    initscr();			/* Start curses mode 		  */
    printw("\u265B");	/* Print Hello World		  */
    refresh();			/* Print it on to the real screen */
    getch();			/* Wait for user input */
    endwin();			/* End curses mode		  */
    

    srand (time(NULL));
    
    ChessWorm worm;
    
    ifstream pgn;
    
    string fname = string(argv[1]);
    
    pgn.open(fname);
    
    if(pgn.is_open()) {
        
        while(!pgn.eof()) {
            worm.games.push_back(new Game(pgn));
            
            if (worm.games.back()->root) {
                worm.roots.push_back(worm.games.back()->root);
            }
        }
    }
    
    else {
        cout << "Test file failed to open" << endl;
    }
    
    pgn.close();

    worm.init();
    
}
