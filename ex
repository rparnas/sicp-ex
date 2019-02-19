#!/bin/bash

# Remove any old "ex-to-load" files lying around.
rm -f "ex-to-load"
rm -f "interpreter"

while true
do
  # Run the interpreter once, loading the file "ex.ss"
  if [ "$1" = "racket" ]; then
    echo "\"racket\"" > interpreter
    racket --load ex.ss --repl
  elif [ -z "$1" ]; then
    echo "\"scheme\"" > interpreter
    scheme ex.ss
  else
    echo "$1" > interpreter
    $1 ex.ss
  fi

  # If file "ex-to-load" is present this means the user wants to run a fresh
  # interpreter with the context of a new exercise. "ex.ss" will load the
  # exercise file indicated by the contents of "ex-to-load".
  if [ ! -f "ex-to-load" ]; then
    break
  fi
done
