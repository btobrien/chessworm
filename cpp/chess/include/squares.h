
#pragma once

#include <string>

const int NUM_SQUARES = 64;
enum { a1, b1, c1, d1, e1, f1, g1, h1,
	   a2, b2, c2, d2, e2, f2, g2, h2,
	   a3, b3, c3, d3, e3, f3, g3, h3,
	   a4, b4, c4, d4, e4, f4, g4, h4,
	   a5, b5, c5, d5, e5, f5, g5, h5,
	   a6, b6, c6, d6, e6, f6, g6, h6,
	   a7, b7, c7, d7, e7, f7, g7, h7,
	   a8, b8, c8, d8, e8, f8, g8, h8, nullsquare = -1 };

bool isSquare(int square);
char file(int square);
char rank(int square);
int toSquare(char file, char rank);
std::string toString(int square);
bool isFile(char);
bool isRank(char);

int lineDirection(int oldSquare, int newSquare);

const int UP  = 8;
const int RIGHT  = 1;
const int DOWN = -8;
const int LEFT  = -1;

const int UP_UP  = 16;
const int DOWN_DOWN = -16;

const int UP_RIGHT  = UP + RIGHT;
const int DOWN_RIGHT = DOWN + RIGHT;
const int DOWN_LEFT  = DOWN + LEFT;
const int UP_LEFT  = UP + LEFT;

const int UP_UP_RIGHT = UP + UP + RIGHT;
const int UP_RIGHT_RIGHT = UP + RIGHT + RIGHT;
const int DOWN_RIGHT_RIGHT = DOWN + RIGHT + RIGHT;
const int DOWN_DOWN_RIGHT = DOWN + DOWN + RIGHT;
const int DOWN_DOWN_LEFT = DOWN + DOWN + LEFT;
const int DOWN_LEFT_LEFT = DOWN + LEFT + LEFT;
const int UP_LEFT_LEFT = UP + LEFT + LEFT;
const int UP_UP_LEFT = UP + UP + LEFT;
