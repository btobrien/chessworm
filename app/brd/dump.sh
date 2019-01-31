#!/bin/bash

disp=${BOARD:-board}

read fen

if [ -f flip.bool ]; then
    $disp <<<"$fen" | flip
else
    $disp <<<"$fen"
fi

