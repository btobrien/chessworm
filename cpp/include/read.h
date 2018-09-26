#pragma once

#include <iostream>
#include <string>
#include <sstream>

std::string getword(std::stringstream& ss) {
	std::string wrd;
	ss >> wrd;
	return wrd;
}

bool try_getword(std::stringstream& ss, std::string& outString) {
	outString = getword(ss);
	return !outString.empty();
}

