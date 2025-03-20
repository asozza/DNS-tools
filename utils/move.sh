#!/bin/bash

dest=some/folder/

for i in {0..3..2}
do
   
    if [ $i -lt 100 ]
    then
	if [ $i -lt 10 ]
	then
	    data='*.00'${i}'.*'
	else
            data='*.0'${i}'.*'
	fi
    else
        data='*.'${i}'.*'
    fi
    rsync -av $data $dest

done
