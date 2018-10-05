#!/bin/bash

id=$1

config="$HOME/.config/brdsrv"
mkdir -p $config
game=$config/$id
touch $game

cat $game
fen <$game
