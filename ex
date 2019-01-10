#!/bin/bash

# Remove any old "n" files lying around.
rm -f "n"

while true
do
  # Run the interpreter once, loading the file "ex.ss"
  if [ "$1" = "racket" ]; then
    racket --load ex.ss --repl
  elif [ -z "$1" ]; then
    scheme ex.ss
  else
    $1 ex.ss
  fi

  # If file "n" is present this means the user wants to run
  # a fresh interpreter and "ex.ss" is expected to load the
  # exercise file indicated by the contents of "n".
  if [ ! -f "n" ]; then
    break
  fi
done
