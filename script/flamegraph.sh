#!/bin/bash

# usage()
# {
#   echo "Usage: $0 PROJECT"
#   exit 2
# }

# proj=$1

# [ -z "$proj" ] && usage

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

cabal run ${proj} -- +RTS -p -RTS
cat ${proj}.prof | ghc-prof-flamegraph > ${proj}.svg

mv ${proj}.prof $srcdir
mv ${proj}.svg $srcdir
