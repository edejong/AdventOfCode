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
echo "Year: $year";
echo "Day: $day";
echo "Title: $title";

[ -z "$year" ] || [ -z "$day" ] || [ -z "$title" ] && usage

mkdir -p $year/Day$day
touch $year/data/day$day.txt
touch $year/data/day$day-test.txt
cat >$year/Day$day/"$title".hs <<EOL
module Day${day}.${title} where

main :: IO ()
main = do
    xs <- readFile "${year}/data/day${day}-test.txt"
    print xs
EOL

module="Day${day}.${title}"

insert_at_hook()
{
  filepath=$1
  hook=$2
  line=$3
  cp $filepath ${filepath}.backup
  sed "s/\([[:space:]]*\)${hook}/\1${line}\n\1${hook}/g" ${filepath}.backup > $filepath
  rm ${filepath}.backup
}

insert_at_hook aoc2021.cabal "-- !!aoc${year} module hook!! --" ", ${module}"
insert_at_hook $year/Main.hs "-- !!aoc${year} import hook!! --" "import qualified ${module}"
insert_at_hook $year/Main.hs "-- !!aoc${year} main hook!! --" ", ${module}.main"
