#!/bin/bash

cd $(dirname $0)

red='\033[0;31m'
green='\033[0;32m'
norm='\033[0m' # No Color

function restore {
	printf "$norm"
}
trap "restore" EXIT

failures=0

for test in $(ls cases); do
	if [ -f cases/$test/init ]; then
		init=$(cat cases/$test/init)
	else
		init=''
	fi

	printf "$green"
	echo [$test]
	printf "$red"
	if ! diff cases/$test/output <(cut -d# -f1 <cases/$test/input | brd "$init"); then
		((failures++))
	fi
done

exit $failtures
