#include "Glyph.h"

const std::string Glyph::NAG[NUM_NAGS] = {
	"", "!", "?", "!!", "??", "!?", "?!",
	"□", "< singular move >", "< worst move >",
	"=", "< equal chances, quiet position >",
	"< equal chances, active position >",
	"∞", "+=", "+=", "+-", "-+", "+−", "−+"
};
