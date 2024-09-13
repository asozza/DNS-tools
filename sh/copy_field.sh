#!/bin/zsh

ifr=090
jfr=180
npe=255

for field in ux uy uz te
do
    for ipe in $(seq -f "%03g" 0 $npe)
    do
        orig=$field'.'${ifr}'.'${ipe}
        dest='New/'$field'.'${jfr}'.'${ipe}	
	cp $orig $dest
    done
done

