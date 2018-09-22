
#include "Tree.h"

Edge::Edge(Node* parentIn, Node* childIn, const GameNode& input):
	parent(parentIn),
	child(childIn),
	AnnotatedMove(input)
{
    child->AddParent(this);
}

Edge::~Edge() {
    delete child;
}

bool Edge::TryPromote() {
	return parent->TryPromote(this);
}

void Node::Connect(Node& child, const GameNode& input) {
    for (auto edge : children) {
        if (edge->child == &child) {
			edge->AddComment(input);
            return;
        }
    }
    AddChild(new Edge(this, &child, input));
}

void Node::AddChild(Edge* child) {
    children.push_back(child);
    if (Strength(child->glyph) > Strength(children[0]->glyph)) { // TODO: priority queue
        children[children.size() - 1] = children[0];
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

Node& Tree::FindOrCreateNode(const string& nodeKey) {
    Node* child = _nodeMap[nodeKey];
    if (!child) {
		Logger::log("Tree::FindOrCreateNode creating node: " + nodeKey);
        child = new Node();
        _nodeMap[nodeKey] = child;
    }
	return *child;
}

bool Tree::CreateEdge(Node& parent, const GameNode* input, Board board) {
    if (!input)
        return false;
	Logger::log("Tree::CreateEdge calling Board::TryMove with " + input->move);
    if (!board.TryMove(input->move)) { 
		Logger::log("Tree::CreateEdge received invalid move: " + input->move, Logger::INPUT);
        return true;
	}
	Node& child = FindOrCreateNode(board.key());
    parent.Connect(child, *input);
    for (auto stepChild : input->variations)
        CreateEdge(child, stepChild, board);
    CreateEdge(child, input->child, board);
	return true;
}

void Tree::Reset() {
	_prevNode = nullptr;
	_currentNode = &_root;
}

void Tree::Invalidate() {
	_prevNode = nullptr;
	_currentNode = nullptr;
}

bool Tree::TrySet(string nodeKey) {
	Logger::log("Tree::TrySet start");
	if (nodeKey.empty()) {
		Reset();
		Logger::log("Tree::TrySet reset the tree to root");
		return true;
	}
	auto result = _nodeMap.find(nodeKey);
	if (result == _nodeMap.end()) {
		Invalidate();
		Logger::log("Tree::TrySet failed to find nodeKey", Logger::WARN);
		return false;
	}
	_prevNode = _currentNode;
	_currentNode = result->second;
	return true;
	// TODO: implement transposition search
}
	
bool Tree::TrySet(string oldPositionKey, string newPositionKey) {
	return TrySet(oldPositionKey) && TrySet(newPositionKey);
}
	
