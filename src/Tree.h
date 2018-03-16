#pragma once

#include <vector>
#include <unordered_map>
#include "Parse.h"
#include "Board.cpp"

using std::string;

struct AnnotatedMove;
struct Node;
struct Edge;
std::vector<int> dummy;

class Tree {
public:
    
    template <typename GameContainer>
    Tree(const GameContainer& games) {
        Board board;
        for (auto game : games) {
            for (auto varRoot: game->varRoots) {
                CreateEdge(_root, varRoot, board);
            }
            CreateEdge(_root, games[i]->root, board);
        }
    }

	void Reset();
	bool TrySet(string nodeKey);

	template <typename Container>
	void GetNextMoves(Container& container) {
		cpy(_currentNode->childEdges, container);
		// TODO: implement transposition search
	}

	AnnotatedMove* GetCurrentMove() {
		return CurrentEdge();
	}


	bool TryPromote() {
		Edge* currentEdge = CurrentEdge();
		return currentEdge ? currentEdge->TryPromote() : false;
	}
		
	template <typename Container>
	void GetGames(Container& container) {
		cpy(dummy, container);
	}

	// TODO: Bookmark Logic

private:
	Node _root;
    Node* _currentNode;
	Node* _prevNode;
    unordered_map<string, Node*> _nodeMap;
	std::vector<const GameNode*> _invalidMoves;
    
	Edge* CurrentEdge();
    void CreateEdge(Node* parent, const GameNode* input, Board &board);
};

struct AnnotatedMove {
    AnnotatedMove(string moveIn) : move(moveIn) {}
    const string move;
	int glyph;
	double eval; // TODO
};

struct Edge : AnnotatedMove, Commentable {
    Edge(Node* childIn, Node* parentIn, const GameNode& input);
    ~Edge();
    Node* child;
    Node* parent;
	bool TryPromote();
	void UpdateGlyph(int candidate) {
		if (Intrigue(candidate) > Intrigue(glyph))
			glyph = candidate;
		return;
	}
private:
	static int Intrigue(int glyph) { return glyph; } // TODO
};

struct Node : Commentable {
    ~Node();
    void Connect(Node& child, const GameNode &input);
    void AddParent(Edge* parent);
    void AddChild(Edge* child);
	std::vector<Edge*> parents;
	std::vector<Edge*> children; // TODO: keep stable
	std::vector<Game*> games;
	bool TryPromote(Edge* child);
private:
	static int Strength(int glyph) { return glyph; } // TODO
};

