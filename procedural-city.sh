#!/bin/bash
TMP_FILE=`mktemp -u`
echo $TMP_FILE
./pc-backend.sh $* -m > $TMP_FILE
./render.sh $TMP_FILE
