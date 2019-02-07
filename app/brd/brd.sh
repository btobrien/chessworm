#!/bin/bash

cd ~/.config/brd/brds || exit 1

cmd=${1:-cat}
name=$2

if [ "$cmd" == 'ls' ]; then
    [ -n "$(ls -A ~/.config/brd/brds)" ] || exit 1
    find . -type d -print | tail -n+2 | sed 's/.\///'
    exit
fi

if [ "$name" == '' ]; then
    if ! ~/.config/brd/brd.sh ls >/dev/null; then
        echo 'error: there are no brds' >&2
        exit 1
    fi
    latest=$(find . -type f -print0 | xargs -0 stat -f "%m %N" | sort -rn | head -1 | cut -f2- -d" ")
    name=$(dirname "$latest")
fi
    
if [ "$cmd" == 'new' ]; then
    mkdir -p ~/.config/brd/brds/$name || exit 1
    cd ~/.config/brd/brds/$name
    touch state.fen
    exit
fi

if ! cd ~/.config/brd/brds/$name 2>/dev/null; then
    echo "error: brd=$name not found" >&2 && exit 1
fi

if [ "$cmd" == 'show' ]; then
    ~/.config/brd/show.sh <state.fen
elif [ "$cmd" == 'flip' ]; then
    rm flip.bool 2>/dev/null || touch flip.bool
elif [ "$cmd" == 'set' ]; then
    cat >state.fen
elif [ "$cmd" == 'cat' ]; then
    ~/.config/brd/dump.sh <state.fen
elif [ "$cmd" == 'fen' ]; then
    sed 's/_/ /g' <state.fen
elif [ "$cmd" == 'who' ]; then
    grep '_w_' <state.fen >/dev/null && echo 'white' || echo 'black'
elif [ "$cmd" == 'wc' ]; then
    cut -d'_' -f6 <state.fen
elif [ "$cmd" == 'tail' ]; then
    rev <state.fen | cut -d'_' -f1 | rev
elif [ "$cmd" == 'castles' ]; then
    cut -d'_' -f3 <state.fen
elif [ "$cmd" == 'passant' ]; then
    cut -d'_' -f4 <state.fen
elif [ "$cmd" == 'white' ]; then
    rm flip.bool 2>/dev/null
elif [ "$cmd" == 'black' ]; then
    touch flip.bool
elif [ "$cmd" == 'watch' ]; then
    stty -echo
    trap "stty echo;" EXIT
    tail -F -n+1 state.fen | ~/.config/brd/watcher.sh
elif [ "$cmd" == 'touch' ]; then
    touch state.fen
elif [ "$cmd" == 'update' ]; then
    cp state.fen .tmp 
    cat .tmp >state.fen
elif grep 'pieces=' >/dev/null <<<$cmd; then
    pieces=$(cut -d'=' -f2 <<<$cmd)
    cp ~/bin/$pieces ~/bin/board
elif [ "$cmd" == 'invert' ]; then
    rm invert.bool 2>/dev/null || touch flip.bool
elif [ "$cmd" == 'delete' ]; then
    rm -r ~/.config/brd/brds/$name
else
    echo "error: command=$cmd not recognized" >&2 && exit 1
fi
