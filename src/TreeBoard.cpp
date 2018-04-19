#pragma once

#include "Tree.h"
#include "Board.cpp"

class TreeBoard : public Tree, public Memory<Board> {
    
public:

    template <typename GameContainer>
    TreeBoard(const GameContainer& games) : Tree(games) {}

    virtual bool TryMove(const std::string& move) {
		if (!MemoryBoard::TryMove(move)) {
			Logger::log("TreeBoard::TryMove received invalid move");
			return false;
		}
		UpdateTree();
		Logger::log("TreeBoard::TryMove updated tree");
		return true;
	}

	virtual bool TryUndo() {
		if (!Memory<Board>::TryUndo()) {
			Logger::log("TreeBoard::TryUndo failed");
			return false;
		}
		UpdateTree();
		Logger::log("TreeBoard::TryUndo updated tree");
		return true;
	}

	virtual bool TryRedo() {
		if (!Memory<Board>::TryRedo()) {
			Logger::log("TreeBoard::TryRedo received invalid move");
			return false;
		}
		UpdateTree();
		Logger::log("TreeBoard::TryRedo updated tree");
		return true;
	}

protected:
	void UpdateTree() {
		Logger::log("TreeBoard::UpdateTree start");
		Tree::TrySet(PrevKey(*this), key());
	}

};


