#pragma once

#include <iostream>
#include <string>

std::string getword(std::string& str) {
	int i = str.find(' ');
	if (i == std::string::npos) {
		std::string wrd = str;
		str = "";
		return wrd;
	}
	std::string wrd = str.substr(0, i);
	while(i < str.length() && str[i] == ' ') { i++; }
	str = str.substr(i);
	return wrd;
}
