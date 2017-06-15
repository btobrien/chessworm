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


void ChessWorm::visit(GameNode* gn, int clock, string indent = "", bool main) {
    
    if (!parseCommand(gn, clock, indent)) {
        return;
    }
    
    int size = 0;
    
    if (gn->parent) {
        size = gn->parent->stepChildren.size();
    }

    if (variations && main) {
        for (int i = 0; i < size; i++) {
            string newIndent = indent + vid[i % 3];
            cout << endl;
            tab();
            cout << "variation" << endl;
            tab();
            cout << "---------" << endl;

            visit(gn->parent->stepChildren[i], clock, newIndent, false);
            cout << endl;
            tab();
            cout << "return" << endl;
            tab();
            cout << "------" << endl;

        }
    }
    
    if (gn->child)
        visit(gn->child, clock + 1, indent);

}


bool ChessWorm::parseCommand(GameNode* gn, int clock, string indent, bool main) {
    
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
            return parseCommand(gn, clock, indent, main);
            
        case 'v':
            getchar();
            variations = !variations;
            return parseCommand(gn, clock, indent, main);
            
        case 's':
            getchar();
            return false;
            
        case 'n':
            getchar();
            currentGame++;
            currentGame = currentGame % games.size();
            return false;
            
        case 'j':
            cin >> currentGame;
            getchar();
            currentGame = currentGame % games.size();
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
                return parseCommand(gn, clock, indent, main);
            }
    }
}


void ChessWorm::study(int game) {
    
    currentGame = game;
    
    cout << games[game]->name << endl;
    cout << games[game]->white << " - " << games[game]->black << endl;
    //cout << games[game]->date << endl;
    cout << games[game]->opening << endl;


    if (annotations) {
        cout << endl << games[game]->intro << endl;
    }
    
    visit(games[game]->root, 0);
    
    getchar();
    
    cout << endl << "---------------------------------";
    cout << endl << "---------------------------------" << endl << endl;

    
    if (currentGame != game) {
        study(currentGame);
    }
    
    currentGame++;
    currentGame = currentGame % games.size();
    
    study(currentGame);
}

ChessWorm::ChessWorm() : annotations(true), variations(true), currentGame(0) {}



