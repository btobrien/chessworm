#!/usr/bin/env python
import sys

BOARD_LENGTH = 8

def isRowDelim(char):
	return (char == '/' or char == ' ' or char == '_')

def DisplaySquare(piece):
	if piece == ' ':
		piece = '-'
	print piece,

def DisplayFen(fen):
	i = 0
	for row in range(BOARD_LENGTH):
		sys.stdout.write(' ')
		char = fen[i]
		i += 1
		while(not isRowDelim(char)):
			if (not char.isdigit()):
				DisplaySquare(char);
			else: 
				for j in range(int(char)):
					DisplaySquare(' ')
			char = fen[i]
			i +=1
		sys.stdout.write(' ')
		print	


if  __name__ == "__main__":
	for line in sys.stdin:
		DisplayFen(line)
