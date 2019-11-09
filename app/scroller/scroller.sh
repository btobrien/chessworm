#!/bin/bash

buffer=$1
[ "$buffer" == '' ] && exit 1
length=$(wc -l <$buffer)
height=$(tput lines)
((height--))
index=1
cursor=1
stty -echo
trap "reset" EXIT
clear
head -n$height <$buffer
tput cup 0 0

while read -n1 key; do
    if [ "$key" == 'j' ]; then
        (( $index == $length )) && continue
        ((index++))
        if (( $cursor < $height )); then
            ((cursor++)) && tput ind
        else
            echo
            head -n$index <$buffer | tail -n1
            tput ri
        fi
    elif [ "$key" == 'k' ]; then
        (( $index == 1 )) && continue
        ((index--))
        if (( $cursor > '1' )); then
            ((cursor--)) && tput ri
        else
            tput ri
            # can probably make faster with awk
            head -n$index <$buffer | tail -n1
            tput ri
        fi
    else
        exit
    fi
done
