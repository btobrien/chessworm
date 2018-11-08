#!/bin/bash

fn=$1
if [ "$fn" == '' ]; then
    echo 'ERROR: too few args provided' >&2
    echo "usage: $0 func" >&2
fi

while read line; do
    tr ' ' '\n' <<<$line | "$fn" | tr '\n' ' '
    echo
done

