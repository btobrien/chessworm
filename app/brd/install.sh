#!/bin/bash

cd $(dirname $0)
mkdir -p  ~/.brd/brds
chmod +x *.sh
cp *.sh ~/.brd/
cp ./brd.sh ~/bin/brd

