
#include "Tree.h"
#include "Board.cpp"

class TreeBoard : public Tree, public MemoryBoard {
    
public:

    template <typename GameContainer>
    TreeBoard(const GameContainer& games) : Tree(games) {}

    virtual bool TryMove(const std::string& move) {
		if (!MemoryBoard::TryMove(move))
			return false;
		UpdateTree();
		return true;
	}

	virtual bool TryUndo() {
		if (!MemoryBoard::TryUndo())
			return false;
		UpdateTree();
		return true;
	}

	virtual bool TryRedo() {
		if (!MemoryBoard::TryRedo())
			return false;
		UpdateTree();
		return true;
	}

	// TODO

    virtual bool Branch();
    virtual bool TryReturn();
    virtual bool TryUnreturn();

    virtual bool TrySet(std::string positionKey);
    virtual bool TryJump(string name);

protected:
	void UpdateTree() {
		TrySet(prev_key(), key());
	}

};


