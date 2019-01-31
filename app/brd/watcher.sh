#!/bin/bash

tput smcup
tput civis

trap "tput cnorm; tput rmcup" EXIT

while read input; do
    tput cup 0 0
    ~/.config/brd/show.sh
done

