
#include "OpeningTree.h"

Edge::Edge(Node* par, Node* chi, const GameNode& input) : parent(par),
											   			  child(chi),
														  move(input->move),
														  glyph(input->glyph)
{
    parent->AddChild(this);
    child->AddParent(this);
}

Edge::~Edge() {
    delete child;
}

void Node::AddChild(Edge* child) {
    childEdges.push_back(child);
    if (Priority(child->glyph) > Priority(childEdges[0]->glyph)) { // TODO: keep sorted?
        childEdges.back() = childEdges[0];
        childEdges[0] = child;
    }
}

void Node::AddParent(Edge* parent) {
    parentEdges.push_back(parent);
}

void Node::AddComment(string commentIn) {
    comment += ( " | " + commentIn); // TODO: format with move and game info
}

Node::~Node() {
    for (auto p : parentEdges)
        p->child = nullptr;
    for (auto c : childEdges)
        delete c;
}

void OpeningTree::CreateEdge(Node* parent, const GameNode* input, Board board) {
    
    if (!input)
        return;
    
    if (!board.TryMove(input->move)) { // TODO: alert something 
		//InvalidMoves.push(input);
        return;
	}
    
    string positionKey = board.Key();
    
    Node* child = _positionMap[positionKey];
    
    if (!child) {
        child = new Node();
        _positionMap[positionKey] = child;
    }

	child->AddComment(input->comment);
    Connect(parent, child, *input);


    for (auto stepChild : input->stepChildren)
        CreateEdge(child, stepChild, board);

    CreateEdge(child, input->child, board);
}

void OpeningTree::Connect(Node* parent, Node* child, const GameNode& input) {
    for (auto edge : parent->childEdges) {
        if (edge->child == child) {
            edge->glyph = max(edge->glyph, input.glyph);
            return;
        }
    }
    new Edge(parent, child, input);
}

OpeningTree::~OpeningTree() {
    delete _root;
}

void OpeningTree::Reset(const Board* position) {
    _currentNode = _root;
}

bool OpeningTree::TrySet(string positionKey) {
	auto result = _positionMap.find(positionKey);
	if (result == _position.end())
		return false;
	_currentNode = result->second;
	return true;
	// TODO: implement transposition search
}

int OpeningTree::Priority(int glyph) { return glyph; } // TODO
	


