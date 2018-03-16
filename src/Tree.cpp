
#include "Tree.h"

Edge::Edge(Node* parentIn, Node* childIn, const GameNode& input) :
	parent(parentIn),
	child(childIn),
	move(input.move)
{
    child->AddParent(this);
}

Edge::~Edge() {
    delete child;
}

bool Edge::TryPromote() {
	return parent->TryPromote(this);
}

void Node::Connect(Node* child, const GameNode& input) {
    for (auto edge : children) {
        if (edge->child == child) {
			edge->AddComment(input.comment, input.glyph, input.author);
            return;
        }
    }
    AddChild(new Edge(this, child, input));
}

void Node::AddChild(Edge* child) {
    children.push_back(child);
    if (Strength(child->glyph) > Strength(children[0]->glyph)) { // TODO: priority queue
        children[childeEdges.size() - 1] = children[0];
        children[0] = child;
    }
}

void Node::AddParent(Edge* parent) {
    parents.push_back(parent);
}

bool Node::TryPromote(Edge* favorite) {
	for (int i = 0; i < children.size(); i++) {
		if (children[i] == favorite) {
			children[i] = children[0];
			children[0] = favorite;
			return true;
		}
	}
	return false;
}

Node::~Node() {
    for (auto p : parents)
        p->child = nullptr;
    for (auto c : children)
        delete c;
}

Node& Tree::FindOrCreateNode(string nodeKey) {
    Node* child = _nodeMap[nodeKey];
    if (!child) {
        child = new Node();
        _nodeMap[nodeKey] = child;
    }
	return *child;
}

void Tree::CreateEdge(Node& parent, const GameNode* input, Board board) {
    if (!input)
        return;
    if (!board.TryMove(input->move)) { 
		_invalidMoves.push(input);
        return;
	}
	FindOrCreate(node board.Key());
    parent.Connect(child, *input);
    for (auto stepChild : input->variations)
        CreateEdge(child, stepChild, board);
    CreateEdge(child, input->child, board);
}

void Tree::Reset() {
	_prevNode = nullptr;
	_currentNode = &_root;
}

bool Tree::TrySet(string nodeKey) {
	if (nodeKey.empty()) {
		Reset();
		return true;
	}
	auto result = _nodeMap.find(nodeKey);
	if (result == _node.end())
		return false;
	_prevNode = _currentNode;
	_currentNode = result->second;
	return true;
	// TODO: implement transposition search
}
	
bool Tree::TrySet(string oldPositionKey, string newPositionKey) {
	return TrySet(oldPositionKey) && TrySet(newPositionKey);
}
	
