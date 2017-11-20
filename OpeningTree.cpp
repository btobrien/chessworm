
#include "OpeningTree.hpp"

Edge::Edge(Node* p, Node* c, const GameNode &input) : parent(p),
												      child(c),
												      AnnotatedMove(input) 
{
    parent->AddChild(this);
    child->AddParent(this);
}

Edge::~Edge() {
    delete child;
}

void Node::AddChild(Edge* child) {
    childEdges.push_back(child);
    if (child->glyph > childEdges[0]->glyph) {
        childEdges.back() = childEdges[0];
        childEdges[0] = child;
    }
}

void Node::AddParent(Edge* parent) {
    parentEdges.push_back(parent);
}

Node::~Node() {
    for (auto p : parentEdges)
        p->child = nullptr;
    for (auto c : childEdges)
        delete c;
}

void OpeningTree::CreateEdge(Node* parent, const GameNode* input, Board &board) {
    
    if (!input)
        return;
    
    if (!board.TryMove(input->move))
        return;
    
    string boardString = board.ToString();
    
    Node* child = _positionMap[boardString];
    
    if (!child) {
        child = new Node();
        _positionMap[boardString] = child;
    }

    Connect(parent, child, *input);

    for (auto stepChild : input->stepChildren)
        CreateEdge(child, stepChild, board);
    CreateEdge(child, input->child, board);
    
    board.UndoMove();
}

void OpeningTree::Connect(Node* parent, Node* child, const GameNode& input) {
    for (auto edge : parent->childEdges) {
        if (edge->child == child) {
            edge->glyph = max(edge->glyph, input.glyph);
            edge->comments.push_back(input.comment);
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
    _path.clear();
}

bool OpeningTree::Traverse(const string& move) {
    for (auto edge : _currentNode->childEdges) {
        if (edge->text == move) {
            Traverse(*edge);
            return true;
        }
    }
    return false;
}

const AnnotatedMove* OpeningTree::GetTopMove() {
    if (_currentNode->childEdges.empty())
        return nullptr;
    Edge* topEdge = _currentNode->childEdges[0];
    Traverse(*topEdge);
    return topEdge;
}

const AnnotatedMove* OpeningTree::GetRandomMove() {
    if (_currentNode->childEdges.empty())
        return nullptr;
    Edge* randomEdge = _currentNode->childEdges[rand() % _currentNode->childEdges.size()];
    Traverse(*randomEdge);
    return randomEdge;
}

void OpeningTree::Traverse(const Edge& edge) {
    _path.push_back(&edge);
    _currentNode = edge.child;
}

bool OpeningTree::Back() {
    if (_path.empty())
        return false;
    _currentNode = _path.back()->parent;
    _path.pop_back();
    return true;
}
const AnnotatedMove* OpeningTree::GetPreviousMove() {
    return _path.empty() ? nullptr : _path.back();
}





