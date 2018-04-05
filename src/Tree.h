#pragma once

#include <vector>
#include <unordered_map>
#include "Parse.h"
#include "Board.h"
#include "Comment.cpp"
#include "Log.h"

using std::string;

struct AnnotatedMove;
struct Node;
struct Edge;

struct AnnotatedMove {
    AnnotatedMove(const GameNode& input) : text(input.move), glyph(input.glyph) {}
    const string text;
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
	std::vector<Game*> games;
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
	bool TryPromote(Edge* child);
private:
	static int Strength(int glyph) { return glyph; } // TODO
};

class Tree {
public:
    template <typename GameContainer>
    Tree(const GameContainer& games) : _prevNode(nullptr) {
        Board board;
		_nodeMap[board.key()] = &_root;
		_currentNode = &_root;
        for (auto game : games) {
            for (auto varRoot : game->varRoots) {
                CreateEdge(_root, varRoot, board);
            }
            CreateEdge(_root, game->root, board);
        }
    }

	bool TrySet(string nodeKey);
	bool TrySet(string nodeKey, string move);
	void Reset();
	void Invalidate();

	bool TryGetCurrentMove(AnnotatedMove& moveOut) {
		return CurrentEdge();
	}

	bool TryPromote() {
		Edge* currentEdge = CurrentEdge();
		return currentEdge ? currentEdge->TryPromote() : false;
	}
		
	template <typename Container>
	void GetGames(Container& container) {
		//cpy(dummy, container);
	}

	// TODO: Bookmark Logic

private:
	Node _root;
    Node* _currentNode;
	Node* _prevNode;
	std::unordered_map<string, Node*> _nodeMap;
    
	Edge* CurrentEdge();
    bool CreateEdge(Node& parent, const GameNode* input, Board board);
	Node& FindOrCreateNode(const string& nodeKey);
};

