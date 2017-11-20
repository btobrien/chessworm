//
//  ChessWorm.cpp
//  ChessWorm
//
//  Created by Bret O'Brien on 12/31/16.
//  Copyright © 2016 Bret O'Brien. All rights reserved.
//

#include "ChessWorm.hpp"


#include <cassert>
#include <iostream>

string vid[3] = {"* ", "- ", "+ "};

void ChessWorm::init() {
    
    if (studying) {
        study(currentGame);
    }
    else {
        train();
    }
    
    init();
}



void ChessWorm::sVisit(GameNode* gn, int clock, string indent = "", bool main) {
    
    if (annotations && gn->precomment.length()) {
        cout << endl << endl << gn->precomment << endl << endl;
    }
    
    if (!sParse(gn, clock, indent)) {
        return;
    }
    
    int size = 0;
    
    if (gn->parent) {
        size = gn->parent->stepChildren.size();
    }

    if (variations && main) {
        for (int i = 0; i < size; i++) {
            string newIndent = indent + vid[i % 3];
            
            if (!read) {
                cout << endl;
                tab();
                cout << "variation" << endl;
                tab();
                cout << "---------" << endl;
            }

            sVisit(gn->parent->stepChildren[i], clock, newIndent, false);
            
            if (!read) {
                cout << endl;
                tab();
                cout << "return" << endl;
                tab();
                cout << "------" << endl;
            }
        }
    }
    
    if (gn->child)
        sVisit(gn->child, clock + 1, indent);

}


bool ChessWorm::sParse(GameNode* gn, int clock, string indent, bool main) {
    
    tab();
    string move;
    char c = getchar();
    
    switch (c) {
            
        case '\n':
            dot(clock, indent);
            gn->print(annotations);
            return true;
            
        case 'w':
            getchar();
            annotations = !annotations;
            return sParse(gn, clock, indent, main);
            
        case 'v':
            getchar();
            variations = !variations;
            return sParse(gn, clock, indent, main);
            
        case 'r':
            getchar();
            read = !read;
            return sParse(gn, clock, indent, main);
            
        case 'n':
            getchar();
            return false;
            
        case 'j':
            cin >> currentGame;
            getchar();
            currentGame = currentGame % games.size();
            return false;
            
        case 'p':
            studying = false;
            getchar();
            return false;

        default:
            cin >> move;
            getchar();
            move = c + move;
            
            if (gn->moveMatch(move)) {
                tab();
                cout << "correct" << endl;
                dot(clock, indent);
                gn->print(annotations);
                cout << endl;
                return true;
            }
            else {
                
                tab();
                cout << "incorrect" << endl << endl;
                return sParse(gn, clock, indent, main);
            }
    }
}


void ChessWorm::study(int game) {
    
    if (!studying) {
        return;
    }
    
    getchar();
    
    cout << endl << "------------------------------------------------------------------";
    cout << endl << "------------------------------------------------------------------" << endl << endl;
    
    currentGame = game;
    
    cout << games[game]->name << endl;
    cout << games[game]->white << " - " << games[game]->black << endl;
    //cout << games[game]->date << endl;
    cout << games[game]->opening << endl;


    if (annotations) {
        cout << endl << games[game]->intro << endl;
    }
    
    sVisit(games[game]->root, 0);
    
    if (currentGame != game) {
        study(currentGame);
    }
    
    currentGame++;
    currentGame = currentGame % games.size();
    
    study(currentGame);
}

//invariant: nextNodes not empty

void ChessWorm::tVisit(vector<GameNode*> &nextNodes, int clock) {
    
    tab();
    string move;
    bool match = false;
    char c = getchar();
    
    switch (c) {
            
        case '\n': {
            int r = rand() % nextNodes.size();
            move = nextNodes[r]->move;
            break;
        }
            
        case 's': {
            cout << endl;
            return;
        }
        
        case 'c': {
            //
        }
            
        default: {
            cin >> move;
            getchar();
            move = c + move;
            break;
        }
    }
    
    vector<GameNode*> temp;
    
    for (int i = 0; i < nextNodes.size(); i++) {
        
        GameNode* current = nextNodes[i];
        
        if (current->moveMatch(move)) {
            
            match = true;
            
            if (current->child) {
                temp.push_back(current->child);
            }
            
            for (int j = 0; j < current->stepChildren.size(); j++) {
                temp.push_back(current->stepChildren[j]);
            }
        }
    }
    
    if (!match) {
        tab();
        cout << "no match" << endl << endl;
        return tVisit(nextNodes, clock);
    }
    
    dot(clock);
    cout << move << endl;
    
    nextNodes = temp;
    
    if (!nextNodes.size()) {
        cout << endl << "------------------------------------------------------------------";
        cout << endl << "------------------------------------------------------------------" << endl << endl;
        return;
    }
    
    tVisit(nextNodes, clock + 1);
}


void ChessWorm::train() {
    
    int g = currentGame;
    
    while (g == currentGame) {
        vector<GameNode*> nextNodes = roots;
        tVisit(nextNodes, 0);
    }
    
    study(currentGame);
}

ChessWorm::ChessWorm() : annotations(true), variations(true), read(false), currentGame(0) {}



