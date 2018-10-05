#!/bin/bash

set -o pipefail

cut -d' ' -f1 | fens | tail -n1 || exit 1
