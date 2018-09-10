int FullClock(bool whiteToMove) {
	return (brd.clock() + 1) / 2;
}
int NextFullClock(bool whiteToMove) {
	return (brd.clock() + 2) / 2;
}
string MovePrefix(bool whiteToMove) { 
	string dot = whiteToMove ? ". " : "...";
	return std::to_string(NextFullClock(whiteToMove)) + dot; 
}

