#!/bin/bash

cur=$(pwd)/$0
dir=$(dirname $cur)

RESULT=0

cd $dir
for file in saven/*.scm; do
    echo Testing file: $file
    bash $file
    r=$?
    if [ $r -ne 0 ]; then
	RESULT=$r
    fi
done

if [ $RESULT -ne 0 ]; then
    echo "Test failed!"
fi

exit $RESULT
