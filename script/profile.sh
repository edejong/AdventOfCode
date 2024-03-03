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
# -i0.05  sampling rate

# https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html#rts-options-to-produce-runtime-statistics

cabal run $proj -- +RTS -p -hy -l-agu --machine-readable -t$statsfile -A32M -B -RTS # -I0 -A100M -RTS   -A100m -H100m

cat $proj.prof | ghc-prof-flamegraph > $proj.prof.svg
eventlog2html $proj.eventlog

mv $proj.prof $srcdir
# rm $proj.eventlog
rm $proj.hp
