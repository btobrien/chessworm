#!/bin/bash

read -d' ' id 

config="$HOME/.config/brdsrv"
mkdir -p $config
game=$config/$id

set -o pipefail

read line
move=$(cut -d'/' -f1 <<<$line)
if cat $game <(echo $move) | fen >/dev/null; then
	echo $line >>$game
	exit
else
	echo "ERROR: move=$move failed" >&2
	exit 1
fi
