#!/bin/bash

id=$1; shift
game="$id".chs
touch $game

cmd=${1:-position}; shift

set -o pipefail

if [ "$cmd" == 'move' ]; then
	move=$@
	if [ "$move" == '' ]; then
		exit 1
	fi
	if cat $game <(echo $move) | chs; then
		echo $move >>$game
	else
		exit 1
	fi
	exit
fi

if [ "$cmd" == 'position' ]; then
	chs <$game
elif [ "$cmd" == 'who' ]; then
	who=$(chs <$game | cut -d' ' -f2)
	[ "$who" == 'w' ] && echo 'white' || echo 'black'
elif [ "$cmd" == 'tail' ]; then
	flags=${1:-'-n1'}; shift
	tail $flags <$game
elif [ "$cmd" == 'cat' ]; then
	cat $game
else
	exit 1
fi
