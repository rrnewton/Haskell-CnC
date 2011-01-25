#!/bin/bash


for file in "$@";
do
   echo
   echo "    Translating erroneous spec: " $file
   echo "  ---------------------------------------------------"
   make $file
   CODE=$?
   if [ $CODE == 0 ];
   then echo 
        echo
	echo "ERROR: bad spec translation SUCCEEDED (zero error code)!"
        echo
        exit 1
   fi
done
