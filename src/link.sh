#!/bin/sh
for F in `find * -name '*.html'`; do
 printf "Linking $F -> ./OUT/$F..."
 ./linker "./$F" "./$F.linked"
 mv "./$F.linked" "../$F" -u
 printf "done\n"
done
