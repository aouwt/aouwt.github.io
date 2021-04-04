#!/bin/sh
for F in `find * -name '*.html'`; do
 DIR="./OUT/`dirname "$F"`"
 if ! [ -d "$DIR" ]; then
  printf "Making directory $DIR..."
  mkdir "$DIR"
  printf "\r"
 fi
 printf "Linking $F -> ./OUT/$F..."
 ./linker "./$F" "./$F.linked"
 mv "./$F.linked" "../$F" -u
 printf "done\n"
done
