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
statsfile="$srcdir/$proj.stats"

# -hy     Profiling by type
# -hd     Profiling by closure description
# -i0.1   sampling rate

cabal run $proj -- +RTS -i0.05 -p -hy -l-agu -s$statsfile # -I0 -A100M -RTS

cat $proj.prof | ghc-prof-flamegraph > $proj.prof.svg
eventlog2html $proj.eventlog

mv $proj.prof $srcdir
rm $proj.eventlog
rm $proj.hp
