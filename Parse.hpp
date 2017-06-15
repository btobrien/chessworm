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


class Game;
class Node;

void tab();
void dot(int clock, string indent = "");

struct Move {
    
public:
    
    string text;
    
    Move(string text);
    
    char piece();
    int  newSquare();
    bool castlesShort();
    bool castlesLong();
    bool takes();
    bool check();
    char file();
    int rank();
    char oldFile();
    int oldRank();
    bool promoted();
    char newPiece();
    
    int stripGlyph();
};

class GameNode {
    
public:
    
    Move move; //most recent
    
    GameNode* parent;
    GameNode* child;
    vector<GameNode*> stepChildren; //variations
    
    Game* game;
    Node* friendGroup;
    
    int glyph;
    string comment;
    string precomment;
    
    GameNode(GameNode* p, string m, string& text, int& i, string pre);
    ~GameNode();
    
    static void next(string &text, int &i, char d);
    static string parseMove(string &text, int &i);
    
    bool parse(string &text, int &i);
    void print(bool annotations);
    bool moveMatch(string move);

    
};

class Game {
    
public:
    
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
    
    void addTag(string tname, string tval);

    static int nagToInt(string nag);
    static const string Square[64];
    static const int NUM_NAGS = 20;
    static string const NAG[NUM_NAGS];
    
    void print(GameNode* gn);
};



#endif /* Parse_hpp */
