#!/bin/bash

cd ~/.brd/brds

cmd=${1:-cat}
name=$2

if [ "$cmd" == 'ls' ]; then
    find . -type d -print | tail -n+2 | sed 's/.\///'
    exit
fi

if [ "$name" == '' ]; then
    latest=$(find . -type f -print0 | xargs -0 stat -f "%m %N" | sort -rn | head -1 | cut -f2- -d" ")
    name=$(dirname "$latest")
fi
    
if [ "$cmd" == 'new' ]; then
    mkdir -p ~/.brd/brds/$name || exit
    cd ~/.brd/brds/$name
    touch update.fen
    exit
fi

if ! cd ~/.brd/brds/$name 2>/dev/null; then
    echo "error: brd=$name not found" >&2 && exit 1
fi

#TODO: abstract the manager pattern from above

if [ "$cmd" == 'write' ]; then
    cat >update.fen
elif [ "$cmd" == 'show' ]; then
    ~/.brd/display.sh <update.fen
elif [ "$cmd" == 'cat' ]; then
    ~/.brd/dump.sh <update.fen
elif [ "$cmd" == 'watch' ]; then
    stty -echo
    trap "stty echo;" EXIT
    tail -F -n+1 update.fen |  ~/.brd/reader.sh
elif [ "$cmd" == 'flip' ]; then
    rm flip.bool 2>/dev/null || touch flip.bool
elif [ "$cmd" == 'white' ]; then
    rm flip.bool 2>/dev/null
elif [ "$cmd" == 'black' ]; then
    touch flip.bool
elif [ "$cmd" == 'touch' ]; then
    touch update.fen
elif grep 'pieces=' >/dev/null <<<$cmd; then
    pieces=$(cut -d'=' -f2 <<<$cmd)
    cp ~/bin/$pieces ~/bin/board
elif [ "$cmd" == 'invert' ]; then
    rm invert.bool 2>/dev/null || touch flip.bool
elif [ "$cmd" == 'delete' ]; then
    rm -r ~/.brd/brds/$name
else
    echo "error: command=$cmd not recognized" >&2 && exit 2
fi
