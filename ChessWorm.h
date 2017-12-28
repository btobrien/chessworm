//
//  ChessWorm.h
//  ChessWorm
//
//  Created by Bret O'Brien on 1/4/17.
//  Copyright © 2017 Bret O'Brien. All rights reserved.
//

#ifndef ChessWorm_h
#define ChessWorm_h


#include "Parse.hpp"
#include "Board.hpp"


#include <iostream>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <map>
#include <fstream>


#include <string>
#include <stack>

using namespace std;


class Move;


//eventually introduce db class? mvp now
class ChessWorm {
    
public:
    
    bool studying;
    
    bool annotations;
    bool variations;
    bool read;
    
    vector<Game*> games;
    int currentGame;
    
    
    /////////////
    
    vector<GameNode*> roots;
    bool bMistakes;
    bool wMistakes;
    
    //unordered_map<string, Node*> fenMapWhite;
    //unordered_map<string, Node*> fenMapBlack;
    
    
    void init();

    void study(int game = 0);
    void sVisit(GameNode* gn, int clock, string variation, bool main = true);
    bool sParse(GameNode* gn, int depth, string variation, bool main = true);
    

    void train();
    void tVisit(vector<GameNode*> &nextNodes, int clock);
    

    
    ChessWorm();

    
};





#endif /* ChessWorm_h */
