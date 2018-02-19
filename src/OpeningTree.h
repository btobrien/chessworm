#pragma once

#include <vector>
#include <unordered_map>
#include "Parse.h"
#include "Board.h"

using std::string;

struct AnnotatedMove {
    AnnotatedMove(string moveIn, int glyphIn) : move(moveIn), glyph(glyphIn) {}
    string move;
    int glyph;
};

struct Node;

struct Edge : AnnotatedMove {
    Edge(Node* childIn, Node* parentIn, const GameNode& input);
    ~Edge();
    Node* child;
    Node* parent;
    vector<Game*> games; // TODO
};

struct Node {
    ~Node();
    void AddParent(Edge* parent);
    void AddChild(Edge* child);
	void AddComment(const GameNode& input);

    vector<Edge*> parentEdges;
    vector<Edge*> childEdges;
    string comment;
};

class OpeningTree {
public:
    
    template <typename GameContainer>
    OpeningTree(const GameContainer& games) {
		_root = new Node();
        Board board;
        for (auto game : games) {
            for (auto varRoot: game->varRoots) {
                CreateEdge(_root, varRoot, board);
            }
            CreateEdge(_root, games[i]->root, board);
        }
    }
    
    ~OpeningTree();

	virtual bool TrySet(string positionKey);

	template <typename MoveContainer>
	bool TryGetChildren(const MoveContainer& moves);
    void GetComment();

	// TODO
    void GetGames(string move);

    
private:
	Node* _root;
    Node* _current;
    unordered_map<string, Node*> _positionMap; //fen to node
    
    void CreateEdge(Node* parent, const GameNode* input, Board &board);
    static void Connect(Node* parent, Node* child, const GameNode &input);
	int Priority(int glyph) { return glyph; } // TODO
};

