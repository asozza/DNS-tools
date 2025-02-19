#!/bin/bash

# Check if a keyword is provided
if [ "$#" -ne 3 ]; then
    echo " "
    echo " Copy particles "
    echo " ---------------------------------------- "
    echo " "
    echo " Usage: ./copy_part.sh <ifr> <jfr> <npe> "
    echo " "
    echo " ifr: original frame "
    echo " jfr: new frame "
    echo " npe: number of processors "
    echo " "
    exit 1
fi

ifr=$1
jfr=$2
npe=$3

for ipe in $(seq -f "%03g" 0 $npe)
do
    orig='part.'${ifr}'.'${ipe}
    dest='New/part.'${jfr}'.'${ipe}     
    cp $orig $dest
done

