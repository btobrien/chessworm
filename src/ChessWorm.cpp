


// Facade 

class ChessWorm : OpeningTree {
    
public:
    bool TrySet(std::string positionKey);
    bool TryTraverse(const std::string& move);

    bool TryBack();
    bool TryForward();

	char* GetBoard() { return board.data(); }
	std::string GetCurrentMove();
	int GetClock();
	std::string GetHistory();

	// TODO
	std::string GetFen();
	std::string GetBookmarks();
    bool Variation();
    bool TryReturn();
    bool TryUnreturn();
    void Bookmark();
    bool TryOpenBookmark(std::string name);

	UpdateComment(std::string);
	UpdateGlyph(std::string);
	PromoteCurrentMove();


private:
	OpeningTree tree;
	MemoryBoard board;
	

};


