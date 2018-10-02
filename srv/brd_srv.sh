#!/bin/bash

sleep 5
set -o pipefail

read -d' ' id 
game="$id".chs

read line
move=$(cut -d'/' -f1 <<<$line)
if cat $game <(echo $move) | fen >/dev/null; then
	echo $line >>$game
	exit
else
	echo "ERROR: move=$move failed" >&2
	exit 1
fi
