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

class Node {
    
public:
    vector<GameNode*> friends;
    
    int size();
};

//eventually introduce db class? mvp now
class ChessWorm {
    
public:
    
    bool annotations;
    bool variations;
    
    //vector<string> players;
    
    //unordered_map<string, Node*> fenMapWhite;
    //unordered_map<string, Node*> fenMapBlack;
    
    //vector<Node*> whiteRoot;
    //vector<Node*> blackRoot;
    
    vector<Game*> games;
    
    int currentGame;

    void study(int game = 0);
    void visit(GameNode* gn, int clock, string variation, bool main = true);
    bool parseCommand(GameNode*, int depth, string variation, bool main = true);
    
    ChessWorm();

    
};





#endif /* ChessWorm_h */
