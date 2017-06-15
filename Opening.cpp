//
//  Opening.cpp
//  ChessWorm
//
//  Created by Bret O'Brien on 1/21/17.
//  Copyright © 2017 Bret O'Brien. All rights reserved.
//

#include "Opening.hpp"



OpeningNode::OpeningNode(int eco, string nm, string mt): ECO(eco), name(nm), moveText(mt) {}

int ecoConvert(string s) {
    assert(s.length() == 3);
    return ((100 * s[0] - 'A') + (10 * s[1] - '1') + (s[2] - '1'));
}
