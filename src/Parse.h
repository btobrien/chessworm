
#pragma once

#include <stdio.h>
#include <iostream>
#include <vector>
#include <fstream>
#include <cassert>
#include <algorithm>
#include "Glyph.cpp"

//class Parser {
//	Game* CreateGame(std::ifstream& pgn);
//
//private:
//	std::string pgn;
//	int index;
//    static void next(std::string &text, int &i, char delim);
//    bool parse(std::string &text, int &i);
//    std::string parseMove();
//
//	template <class PgnNode>
//	void createNode(PgnNode* parent) 
//};

struct Game;

struct GameNode {
	std::string move;
    int glyph;
	std::string comment;
	std::string precomment;
    GameNode* parent;
    GameNode* child;
	std::vector<GameNode*> variations;
    GameNode(GameNode* p, std::string m, std::string& text, int& i, std::string pre);
    ~GameNode();

private:
    static void next(std::string &text, int &i, char delim);
    bool parse(std::string &text, int &i);
    static std::string parseMove(std::string &text, int &i);

};

struct Tags {
	std::string name; //event tag
	std::string white;
	std::string black;
	std::string date;
	std::string opening;
	std::string annotator;
	std::string result;
};

struct Game {
    GameNode* root;
	std::vector<GameNode*> varRoots; 
	Tags tags
	std::string intro;
    Game(std::ifstream& pgn);
    ~Game();
private:
    void addTag(std::string tname, const std::string& tval);
};


