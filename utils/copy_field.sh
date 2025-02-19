#!/bin/bash

# Check if a keyword is provided
if [ "$#" -ne 3 ]; then
    echo " "
    echo " Copy fields "
    echo " ---------------------------------------- "
    echo " "
    echo " Usage: ./copy_field.sh <ifr> <jfr> <npe> "
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

list=(ux uy uz te)
for field in ${list[*]} # OR ${list[@]}
do
    for ipe in $(seq -f "%03g" 0 $npe)
    do
        orig=$field'.'${ifr}'.'${ipe}
        dest='New/'$field'.'${jfr}'.'${ipe}     
        cp $orig $dest
    done
done

