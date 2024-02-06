#!/bin/bash

usage()
{
  echo "Usage: $0 -y YEAR -d DAY"
  exit 2
}

while getopts y:d:t: flag
do
    case "${flag}" in
        y) year=${OPTARG};;
        d) day=$(printf "%02d\n" ${OPTARG});;
    esac
done

[ -z "$year" ] || [ -z "$day" ] && usage

srcdir="$year/Day$day"
proj="aoc$year-$day"

cabal run $proj -- +RTS -p -hy -l-agu -RTS

cat $proj.prof | ghc-prof-flamegraph > $proj.prof.svg
eventlog2html $proj.eventlog

mv $proj.prof $srcdir
rm $proj.eventlog
rm $proj.hp
