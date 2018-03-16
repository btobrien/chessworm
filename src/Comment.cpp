#include <string>
#include <vector>

using std::vector;
using std::string;

struct Comment {
    string comment;
	string author;
    int glyph;
	// TODO: engine assesment?
};

class ICommentable {
	virtual Comment& GetComment();
	virtual void DeleteComment();
	virtual void NextComment();
	virtual void AddComment(Commente* comment);
};

class Commentable : ICommentable {
	virtual void AddComment(Commente* comment);
protected:
	vector<Comment*> comments;
	int currentIndex;
};


// TODO: author/move filters
