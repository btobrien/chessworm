//
//  reader.cpp
//  ChessWorm
//
//  Created by Bret O'Brien on 1/27/17.
//  Copyright © 2017 Bret O'Brien. All rights reserved.
//

#include "Parse.hpp"

const string Game::NAG[Game::NUM_NAGS] = {"", "!", "?", "!!", "??", "!?", "?!", "□", "< singular move >", "< worst move >", "=", "< equal chances, quiet position >", "< equal chances, active position >", "∞", "+=", "+=", "+-", "-+", "+−", "−+" };


//can make personalized NAGs

const char t = 9;

int Game::nagToInt(string nag) {
    
    for (int i = 0; i < NUM_NAGS; i++) {
        if (nag == NAG[i]) {
            return i;
        }
    }
    
    assert(false);
    return -1;
}

void tab() {
    cout << t << t << t << t << t << t << t << t << t << t << t;
    cout.flush();
}

void dot(int clock, string indent) {
    
    cout << indent << (clock / 2) + 1;
    
    if (clock % 2 == 0) {
        cout << ". ";
    }
    else {
        cout << "... ";
    }
}



/*-----------------------------------------------------------------------*/
/*----------------------------  Move  -----------------------------------*/
/*-----------------------------------------------------------------------*/


Move::Move(string t) : text(t) {}


int  Move::newSquare() {
    
    //currently ignores queening!
    
    int file = text.length() - 2;
    int rank = text.length() - 1;
    
    if (check()) {
        file -= 1;
        rank -=1;
    }

    return (((text[rank] - '1') * 8) + text[file] - 'a');
    
}
char Move::file() {
    return ('a' + newSquare() % 8);
}
int Move::rank() {
    return (1 + newSquare() / 8);
}

char Move::piece() {
    
    if (castlesLong() || castlesShort()) {
        return 'K';
    }
    else if (text[0] >= 'a' && text[0] <= 'h') {
        return 'P';
    }
    return text[0];
}

bool Move::check() {
    return (text.find('+') != string::npos);
}

bool Move::castlesShort() {
    return (text == "O-O");
}

bool Move::castlesLong() {
    return (text == "O-O-O");
}

bool Move::takes() {
    return (text.find('x') != string::npos);
}


char Move::oldFile() {
    return 0;
}
int Move::oldRank() {
    return 0;
}
bool Move::promoted() {
    return false;
}
char Move::newPiece() {
    return 'Q';
}



/*-----------------------------------------------------------------------*/
/*----------------------------  Game Node  -----------------------------*/
/*-----------------------------------------------------------------------*/




//MODIFIES: updates "i" to point to the next instance of d
void GameNode::next(string &text, int &i, char d = ' ') {
    
    while(text[++i] != d) {
        
        assert(i <= text.length());
        
        //ignore closing parentheses when scanning commments
        if (text[i] == ')' && d != '}') {
            return;
        }
    }
}

//INVARIANT: i is initialized to blank space since last
//MODIFIES: i to point to next move
//EFFECTS: returns false if there is no next move

bool GameNode::parse(string &text, int& i) {
    
    char c = text[i];
    
    //base case: last move
    if (text[i] == ')' || text[i + 1] == '*') {
        //cout << endl << endl << " - end - " << endl;
        return false;
    }
    
    assert(text[i] == ' ');
    
    i++;
    int start = i + 1;
    
    if (text[i] == ')' || text[i + 1] == '*') {
       // cout << endl << endl << " - end - " << endl;
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
    
    //recurse: ignore move numbers
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
            //cout << endl << Game::NAG[glyph];

            break;
        }
            
        case '{': {
            
            char c = text[i];
            
            
            next(text, i, '}');
            
            c = text[i];
            
            if (text[start] == ' ')
                start++;
            
            comment = text.substr(start, i - start);
            
            //cout << endl << endl << comment << endl;
            
            if (comment == "1. Read the chapter introductions and illustrative games.") {
                //
            }
            
            next(text, i);
            
            
            c = text[i];


            break;
        }
            
        case '(': {
            
            //cout << endl << endl << " - sideline - " << endl;
            string pre;
            
            if (text[i + 1] == '{') {
                
                start++;
                next(text, i, '}');
                
                if (text[start] == ' ')
                    start++;
                
                pre = text.substr(start, i - start);
                
                //cout << endl << endl << pre << endl;
                
                next(text, i, '.');
            }
            
            next(text, i);
            i++;
            
            string m = parseMove(text, i);

            if (parent) {
                parent->stepChildren.push_back(new GameNode(parent, m, text, i, pre));
            }
            
            char c = text[i];


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
string GameNode::parseMove(string &text, int &i) {
    
    int start = i;
    next(text, i);
    
    return text.substr(start, i - start);
}

GameNode::GameNode(GameNode* p, string m, string& text, int& i, string pre = "") :  move(m), parent(p), glyph(0), precomment(pre) {
    
    //cout << endl << "move: " << m << endl;
    
    if (parse(text, i)) {
        child = new GameNode(this, parseMove(text, i), text, i);
    }
}

GameNode::~GameNode() {
    
    delete child;
    
    for (int i = 0; i < stepChildren.size(); i++) {
        delete stepChildren[i];
        stepChildren[i] = nullptr;
    }
}

void GameNode::print(bool annotations) {
    cout << move.text << Game::NAG[glyph] << endl;
    if (annotations && comment.size()) {
        cout << endl << comment << endl;
    }
}

bool GameNode::moveMatch(string m) {
    return m == move.text;
}


/*-----------------------------------------------------------------------*/
/*----------------------------  Game  -----------------------------------*/
/*-----------------------------------------------------------------------*/


Game::Game(ifstream& pgn) {
    
    root = nullptr;
    
    string moveText;
    
    string tname;
    string tval;
    char c = 0;
    string s;
    
    while (c != '[')
        pgn >> c;
    
    
    while(c == '[') {
        
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
        //cout << endl << intro << endl << endl;
        
        GameNode::next(moveText, i, '.');
    }

    GameNode::next(moveText, i);
    i++;
    
    root = new GameNode(nullptr, GameNode::parseMove(moveText, i), moveText, i);

}

Game::~Game() {
    delete root;
}

void Game::addTag(string tname, string tval) {
    
    if (tname == "Event" && tval[0] != '?') {
        name = tval;
    }
    else if (tname == "White" && tval[0] != '?') {
        white = tval;
    }
    else if (tname == "Black" && tval[0] != '?') {
        white = tval;
    }
    else if ((tname == "Date" || tname == "UTCDate") && tval[0] != '?') {
        date = tval;
    }
    else if (tname == "Opening" && tval[0] != '?') {
        opening = tval;
    }
    else if (tname == "Annotator" && tval[0] != '?') {
        annotator = tval;
    }
    return;
}





