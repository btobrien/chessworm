#!/bin/bash

cd $(dirname $0)
mkdir -p  ~/.config/brd/brds
chmod +x *.sh
cp *.sh ~/.config/brd/
cp ./brd.sh ~/bin/brd

