#pragma once
#include <string>
#include "Log.h"

class Glyph {
public:
	static int ToInt(const std::string& nag) {
		for (int i = 0; i < NUM_NAGS; i++) {
			if (nag == NAG[i])
				return i;
		}
		return nullglyph;
	}
	static std::string ToString(int glyph) {
		if (glyph < 0 || glyph >= NUM_NAGS)
			return "";
		return NAG[glyph];
	}

    static const int NUM_NAGS = 20;
	static const int nullglyph = 0;

	static int Strip(std::string& move) {
		size_t len = move.length();
		if (len < 2)
			return nullglyph;
		int glyph = ToInt(move.substr(len - 2));
		if (glyph > 0) {
			move.pop_back();
			move.pop_back();
			return glyph;
		}
		glyph = ToInt(move.substr(len - 1));
		if (glyph > 0) {
			move.pop_back();
		}
		return glyph; 
	}
	
private:
	static const std::string NAG[NUM_NAGS];
};
