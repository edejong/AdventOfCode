#!/bin/bash

usage()
{
  echo "Usage: $0 -y YEAR -d DAY -t TITLE"
  exit 2
}

while getopts y:d:t: flag
do
    case "${flag}" in
        y) year=${OPTARG};;
        d) day=$(printf "%02d\n" ${OPTARG});;
        t) title=${OPTARG};;
    esac
done

[ -z "$year" ] || [ -z "$day" ] || [ -z "$title" ] && usage

title=$(echo $title | tr -cd [:alnum:])
module="Day${day}.${title// /}"
srcdir="$year/Day$day"

echo "Year: $year";
echo "Day: $day";
echo "Title: $title";
echo "Module: $module";

if [[ -d "$srcdir" ]]; then
  echo "(!) Already exists, exiting"
  exit 2
fi

mkdir -p "$srcdir"
touch $srcdir/day$day.txt
touch $srcdir/day$day-test.txt
cat >$srcdir/"$title".hs <<EOL
module ${module} where

main :: IO ()
main = do
    xs <- readFile "${srcdir}/day${day}-test.txt"
    print xs
EOL

insert_at_hook()
{
  filepath=$1
  hook=$2
  line=$3
  cp $filepath ${filepath}.backup
  sed "s/\([[:space:]]*\)${hook}/\1${line}\n\1${hook}/g" ${filepath}.backup > $filepath
  rm ${filepath}.backup
}

insert_at_hook adventOfCode.cabal "-- !!aoc${year} module hook!! --" ", ${module}"
insert_at_hook $year/Main.hs "-- !!import hook!! --" "import qualified ${module}"
insert_at_hook $year/Main.hs "-- !!main hook!! --" ", ${module}.main"
