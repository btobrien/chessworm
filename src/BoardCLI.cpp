

#pragma once
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <vector>
#include <fstream>
#include "Log.h"
#include "Board.h"

using std::string;
using std::vector;
using std::cin;



int main(int argc, const char* argv[]) {
	Logger::open();

	Logger::log("main: succesfully logged");

	string initKey;
	Board brd(init)

	string move;
	
	while(cin >> move && brd.TryUpdate(move)) {}

	if 



	Logger::close();
}





