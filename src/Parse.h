
#pragma once

#include <stdio.h>
#include <iostream>
#include <vector>
#include <fstream>

#include <cassert>
#include <algorithm>

using std::string;

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

//private:
    static void next(string &text, int &i, char delim);
    bool parse(string &text, int &i);
    static string parseMove(string &text, int &i);

    void stripGlyph();
};


struct Game {
    GameNode* root;
    vector<GameNode*> varRoots; 
    
    Game(ifstream& pgn);
    ~Game();
    
    string name; //event tag
    string white;
    string black;
    string date;
    string opening;
    string annotator;
    bool result;

    string intro;
    
    void addTag(string tname, const string& tval);

    static int nagToInt(const string& nag);
    static const int NUM_NAGS = 20;
    static string const NAG[NUM_NAGS];
    
    void Print();
};

