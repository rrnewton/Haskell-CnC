#!/bin/bash

# Return the first of the arguments that is an existing file or directory.

# There could be a way to do this in Make (wildcard? filter? or?), but
# I don't know it so I've factored out this little piece.

for file in "$@"; 
do
  if [ -e "$file" ]; then
     echo $file
     exit 0
  fi
done

echo "first_existing.sh: ERROR - No input files existed out of: $*" > /dev/stderr
exit 1
