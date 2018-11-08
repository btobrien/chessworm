#!/bin/bash

# note: only supports empty prefix tree (i.e. [[]])
sed 's/\[\["//' | sed 's/"\]\]//' | sed 's/\[\[//' | sed 's/\]\]//' | sed 's/"\],\["/\]/g' | sed 's/\],\[/\]/g' | sed 's/","/ /g' | tr ']' '\n'
