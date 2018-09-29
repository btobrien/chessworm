#pragma once

#include <string>

template<typename S>
std::string getword(S& stream) {
	std::string wrd;
	stream >> wrd;
	return wrd;
}

template<typename S>
bool try_getword(S& stream, std::string& outString) {
	outString = getword(stream);
	return !outString.empty();
}

