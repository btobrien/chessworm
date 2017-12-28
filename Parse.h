//
//  reader.hpp
//  ChessWorm
//
//  Created by Bret O'Brien on 1/27/17.
//  Copyright © 2017 Bret O'Brien. All rights reserved.
//

#ifndef Parse_hpp
#define Parse_hpp

#include <stdio.h>
#include <iostream>
#include <vector>
#include <fstream>

using namespace std;

#include <cassert>
#include <algorithm>

struct Game;

struct GameNode {
    string move;
    int glyph;
    string comment;
    string precomment;
    
    GameNode* parent;
    GameNode* child;
    vector<GameNode*> stepChildren;
    
    GameNode(GameNode* p, string m, string& text, int& i, string pre);
    ~GameNode();
    
    static void next(string &text, int &i, char d);
    static string parseMove(string &text, int &i);
    
    bool parse(string &text, int &i);
    void print(bool annotations);
    void stripGlyph();
};

struct Game {
    GameNode* root;
    vector<GameNode*> varRoots; //variations
    
    Game(ifstream& pgn);
    ~Game();
    
    string name; //event tag
    string white;
    string black;
    string date;
    string opening;
    string annotator;
    string intro;
    bool result;
    
    void addTag(string tname, const string& tval);

    static int nagToInt(const string& nag);
    static const int NUM_NAGS = 20;
    static string const NAG[NUM_NAGS];
    
    void Print();
};

#endif /* Parse_hpp */
