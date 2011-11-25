#!/bin/bash

if [ $# -ne 1 ]
then
    echo use: $0 "<size>"
else
    size=$1
fi

echo CONFIRM: $size

echo 2 > matrix.txt 
echo $size >> matrix.txt 
echo $size >> matrix.txt 
echo "" >> matrix.txt

maxelt=`expr $size \* $size` 
 
seq 1 $maxelt >> matrix.txt 
