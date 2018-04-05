
#include "Parse.h"

using std::string;
using std::ifstream;

//MODIFIES: index to point to the next instance of delim
void GameNode::next(const string &text, int& i, char delim = ' ') {
    
    while(text[++i] != delim) {

        assert(i <= text.length()); // parser should handle malformed pgns gracefully

        //ignore closing parentheses when scanning commments
        if (text[i] == ')' && delim != '}')
            return;
    }
}

//INVARIANT: i is initialized to blank space since last
//MODIFIES: i to point to next move
//EFFECTS: returns false if there is no next move

bool GameNode::parse(const string &text, int& i) {
    
    //base case: last move
    if (text[i] == ')' || text[i + 1] == '*') {
		Logger::log("GameNode::parse reached end of pgn line");
        return false;
    }
    
    assert(text[i] == ' ');
    
    i++;
    int start = i + 1;
    
    if (text[i] == ')' || text[i + 1] == '*') {
		Logger::log("GameNode::parse reached end of pgn line");
        return false;
    }
    
    //unintended behavior?? -- shouldn't interfere with castles
    //segfault check??
    if (text [i + 1] == '-' && text[i] != '{' && text[i] != 'O') {
        if (text[i] == '1' || text[i] == '0')
            return false;
    }
    if (text [i + 3] == '-' && text[i] != '{' && text[i] != 'O') {
        if (text[i + 2] == '2')
            return false;
    }
    
    //recurse: ignore move numbers (use is digit func)
    if (text[i] > '0' && text[i] <= '9') {
        next(text, i);
        return parse(text, i);
    }
    
    switch(text[i]) {
            
        case ' ':
            return parse(text, i);
        
        case '$': {
            next(text, i);
            glyph = stoi(text.substr(start, i - start));
			Logger::log("GameNode::parse found glyph: " + Glyph::ToString(glyph));

            break;
        }
            
        case '{': {
            next(text, i, '}');
            while (text[start] == ' ')
                start++;
            comment = text.substr(start, i - start);
            
			Logger::log("GameNode::parse found comment: " + comment);

            next(text, i);
            break;
        }
            
        case '(': {
            
			Logger::log("GameNode::parse creating new sideline");
            string pre;
            
            if (text[i + 1] == '{') {
                
                start++;
                next(text, i, '}');
                
                if (text[start] == ' ')
                    start++;
                
                pre = text.substr(start, i - start);
                
				Logger::log("GameNode::parse found pre-comment: " + pre);
                
                next(text, i, '.');
            }
            
            next(text, i);
            i++;
            
            string m = parseMove(text, i);

            if (parent) {
                parent->variations.push_back(new GameNode(parent, m, text, i, pre));
}
            else {
				Logger::log("GameNode::parse something was ignore...");
                next(text, i, ')');
            }

            assert(text[i] == ')');
            next(text, i);
            break;
        }
            
        default: {
            //base case: new move
            return true;
        }
    }
    
    //recurse
    return parse(text, i);
}

//NOTE: i points to first char of movetext
//REQUIRES: next move exists
string GameNode::parseMove(const string &text, int &i) {
    int start = i;
    next(text, i);
    return text.substr(start, i - start);
}

GameNode::GameNode(GameNode* p, string m, const string& text, int& i, string pre = "") : move(m),
																					     parent(p),
																					     child(nullptr),
																					     precomment(pre)
{
	Logger::log("GameNode::Constructor found new move: " + move);
    if (parse(text, i))
        child = new GameNode(this, parseMove(text, i), text, i);
    glyph = Glyph::Strip(move);
}

GameNode::~GameNode() {
    delete child;
    for (int i = 0; i < variations.size(); i++) {
        delete variations[i];
        variations[i] = nullptr;
    }
}

string GameNode::ToString() {
	std::stringstream out;
    out << move << Glyph::ToString(glyph);
    if (comment.size()) {
        out << ": " << comment;
    }
}

Game::Game(ifstream& pgn) {
    
    root = nullptr;
    
    string moveText;
    
    string tname;
    string tval;
    char c = 0;
    string s;
    
    while (c != '[')
        pgn >> c;
    
    while (c == '[') {
        
        pgn >> tname;
    
        pgn >> c;
        assert(c == '"');

        getline(pgn, tval);
        
        assert(tval[tval.length() - 1] == ']');
        
        tval.pop_back(); //garbage quote
        
        assert(tval[tval.length() - 1] == '"');
        tval.pop_back(); //garbage bracket
        
        addTag(tname, tval);
        pgn >> c;
    }
    
    if (c == '*') {
        return;
    }
    
    //else c should equal 1
    
    getline(pgn, s);

    while(s != "") {
        moveText += c;
        c = ' ';
        moveText += s;
        getline(pgn, s);
    }
    
    int i = 0;
    int start = 1;
    
    if (moveText[0] == '{') {
        GameNode::next(moveText, i, '}');
        if (moveText[start] == ' ')
            start++;
        intro = moveText.substr(start, i - start);
        GameNode::next(moveText, i, '.');
    }

    GameNode::next(moveText, i);
    i++;
    root = new GameNode(nullptr, GameNode::parseMove(moveText, i), moveText, i);
}

Game::~Game() {
    delete root;
}

void Game::addTag(string tname, const string& tval) {
    
    if (tname == "Event" && tval[0] != '?')
        tags.name = tval;
    else if (tname == "White" && tval[0] != '?')
        tags.white = tval;
    else if (tname == "Black" && tval[0] != '?')
        tags.black = tval;
    else if ((tname == "Date" || tname == "UTCDate") && tval[0] != '?')
        tags.date = tval;
    else if (tname == "Opening" && tval[0] != '?')
        tags.opening = tval;
    else if (tname == "Annotator" && tval[0] != '?')
        tags.annotator = tval;
    return;
}





