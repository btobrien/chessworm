
class Glyph {
    static int nagToInt(const std::string& nag);
    static const int NUM_NAGS = 20;
	static int Strip(std::string& move);
	static const std::string NAG[NUM_NAGS];
};

int Glyph::ToInt(const std::string& nag) {
    for (int i = 0; i < NUM_NAGS; i++) {
        if (nag == NAG[i])
            return i;
    }
    return -1;
}

int Glyph::Strip(std::string& move) {
    size_t len = move.length();
    if (len < 2)
        return;
	std::string candidate = move.substr(len - 2);
    int glyph = nagToInt(candidate);
    if (glyph > 0) {
        move.pop_back();
        move.pop_back();
        return;
    }
    candidate = move.substr(len - 1);
    glyph = nagToInt(candidate);
    if (glyph > 0) {
        move.pop_back();
    }
	return glyph; 
}

const std::string Glyph::NAG[NUM_NAGS] = {
	"", "!", "?", "!!", "??", "!?", "?!",
	"□", "< singular move >", "< worst move >",
	"=", "< equal chances, quiet position >",
	"< equal chances, active position >",
	"∞", "+=", "+=", "+-", "-+", "+−", "−+"
};
