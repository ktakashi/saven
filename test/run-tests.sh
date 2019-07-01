#!/bin/bash

set -e

cur=$(pwd)/$0
dir=$(dirname $cur)

cd $dir
for file in saven/*.scm; do
    echo Testing file: $file
    bash $file
done
