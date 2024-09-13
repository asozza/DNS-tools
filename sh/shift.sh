#!/bin/bash

ifr1=1
ifr2=180
npe=255

# loop on fields
for field in ux uy uz te
do
    # loop on frames
    for ifr in $(seq -f "%03g" $ifr1 $ifr2)
    do
        jfr=$((${ifr#0}-$ifr1))
        printf -v jfrs "%03d" $jfr
        # loop on slices
        for ipe in $(seq -f "%03g" 0 $npe)
        do
            orig=$field'.'$ifr'.'$ipe
            dest='New/'$field'.'$jfrs'.'$ipe
            cp $orig $dest
        done # end slices
    done # end frames
done # endfields

