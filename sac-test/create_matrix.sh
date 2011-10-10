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
    
maxelt=`expr $size \* $size` 
 
for elt in `seq 1 $maxelt` 
do 
    echo $elt >> matrix.txt 
done 
