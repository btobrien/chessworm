#!/bin/bash
read line
len=${#line}
mid=$((($(tput cols)-$len)/2))
pad=%"$mid"s
printf "$pad"
echo $line
while read line; do
	printf "$pad"
	echo $line
done
