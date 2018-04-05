#pragma once
#include <string>
#include <vector>

using std::vector;
using std::string;

struct Comment {
	Comment(const GameNode& input):
		text(input.move),
		author(input.game->tags.annotator),
		glyph(input.glyph) {}
    string text;
	string author;
    int glyph;
	// TODO: engine assesment?
};

class ICommentable {
public:
	virtual void AddComment(Comment comment) = 0;
	virtual void DeleteComment() = 0;
	virtual Comment& GetComment() = 0;
	virtual void NextComment() = 0;
};

class Commentable : ICommentable {
public:
	virtual void AddComment(Comment comment) override {}
	virtual void DeleteComment() override {}
	Comment& GetComment() override {}
	virtual void NextComment() override {}
protected:
	vector<Comment*> comments;
	int currentIndex;
};


// TODO: author/move filters
