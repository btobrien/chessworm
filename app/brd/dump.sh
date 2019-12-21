#!/bin/bash

disp=${BOARD:-board}

if [ -f flip.bool ]; then
    $disp <state.fen | flip
else
    $disp <state.fen
fi

