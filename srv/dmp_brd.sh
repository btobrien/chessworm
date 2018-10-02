#!/bin/bash

id=$1
game="$id".chs
touch $game
cat $game
fen <$game
