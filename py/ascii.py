#!/usr/bin/env python

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
		print	


if  __name__ == "__main__":
	import sys
	print
	for line in sys.stdin:
		DisplayFen(line)
	print
