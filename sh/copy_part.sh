#!/bin/bash

ifr=030

for i in {0..63}
do

    if [ $i -lt 100 ]
    then
	if [ $i -lt 10 ]
	then
	    orig='part.'${ifr}'.00'${i}
	    dest='New/part.000.00'${i}
	else
	    orig='part.'${ifr}'.0'${i}
	    dest='New/part.000.0'${i}
	fi
    else
	orig='part.'${ifr}'.'${i}
	dest='New/part.000.'${i}
    fi
    cp $orig $dest
    
done
