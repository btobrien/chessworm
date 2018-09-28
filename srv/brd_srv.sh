#!/bin/bash

id=$1
read cmd; 

if ! ./srv_cmd.sh $id $cmd; then
	echo 'ERROR'
	exit 1
fi
