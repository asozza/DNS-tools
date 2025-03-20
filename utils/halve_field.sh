#!/bin/bash

# Check if a keyword is provided
if [ "$#" -ne 4 ]; then
    echo " "
    echo " Halve fields "
    echo " ---------------------------------------- "
    echo " "
    echo " Usage: ./halve_field.sh ifr jfr npe nscale "
    echo " "
    echo " ifr: original frame "
    echo " jfr: new frame "
    echo " npe: number of processors "
    echo " nscale: rescaling factor "
    echo " "
    exit 1
fi

ifr1=1
ifr2=180
nscale=2
npe=255

# loop on fields
for field in ux uy uz te
do
    # loop on frames
    for ifr in $(seq -f "%03g" $ifr1 $ifr2)
    do
	if [ $(expr $ifr % 2) == 0 ]
           jfr=$(($ifr/$nscale))
           printf -v jfrs "%03d" $jfr
           # loop on slices
           for ipe in $(seq -f "%03g" 0 $npe)
           do
               orig=$field'.'$ifr'.'$ipe
               dest='New/'$field'.'$jfrs'.'$ipe
               cp $orig $dest
           done # end slices
	fi
    done # end frames
done # endfields

