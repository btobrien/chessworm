#!/bin/bash

disp=${BOARD:-board}

read fen

if [ -f flip.bool ]; then
    $disp <<<"$fen" | flip | border | center
else
    $disp <<<"$fen" | border | center
fi

