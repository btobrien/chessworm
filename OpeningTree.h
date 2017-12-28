//
//  OpeningTree.hpp
//  ChessWorm
//
//  Created by Bret O'Brien on 10/22/17.
//  Copyright © 2017 Bret O'Brien. All rights reserved.
//

#ifndef OpeningTree_hpp
#define OpeningTree_hpp

#include <stdio.h>
#include <vector>
#include <list>
#include <unordered_map>
#include "Parse.hpp"
#include "Board.hpp"

using namespace std;

struct AnnotatedMove {
    
    AnnotatedMove(const GameNode& input) : text(input.move), glyph(input.glyph) {
        comments.push_back(input.comment);
    }
    
    string text;
    int glyph;
    vector<string> comments;
};

struct Node;

struct Edge : AnnotatedMove {
    Edge(Node* c, Node* p, const GameNode &input);
    ~Edge();
    Node* child;
    Node* parent;
};

struct Node {
    ~Node();
    void AddParent(Edge* parent);
    void AddChild(Edge* child);
    vector<Edge*> parentEdges;
    vector<Edge*> childEdges;
};

class OpeningTree {
public:
    
    template <typename GameContainer>
    OpeningTree(GameContainer games) {
        Board board;
        for (auto game : games) {
            for (auto varRoot: game->varRoots) {
                CreateEdge(_root, varRoot, board);
            }
            CreateEdge(_root, games[i]->root, board);
        }
    }
    
    ~OpeningTree();
    
    void Reset(const Board* position = nullptr);
    
    bool Traverse(const string& move);
    bool TraverseTop();
    bool TraverseRandom();
    bool Back();

    const AnnotatedMove* GetTopMove();
    const AnnotatedMove* GetRandomMove();
    const AnnotatedMove* GetPreviousMove();
    
    template <typename MovesContainer>
    void GetMoves(MovesContainer& moves) {
        for (auto m : _currentNode->childEdges) {
            moves.insert(moves.end(), (AnnotatedMove*) m);
        }
    }
    
    template <typename MovesContainer>
    void GetPath(MovesContainer& path) {
        for(auto m : _currentNode->childEdges) {
            path.insert(path.end(), (AnnotatedMove*) m);
        }
    }
    
private:
    Node* _root = new Node();
    unordered_map<string, Node*> _positionMap; //fen to node
    Node* _currentNode;
    vector<const Edge*> _path;
    
    static void Connect(Node* parent, Node* child, const GameNode &input);
    void CreateEdge(Node* parent, const GameNode* input, Board &board);
    void Traverse(const Edge& edge);
};



#endif /* OpeningTree_hpp */
