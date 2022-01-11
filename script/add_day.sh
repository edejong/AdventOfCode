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

title=${title// /}
module="Day${day}.${title// /}"

echo "Year: $year";
echo "Day: $day";
echo "Title: $title";
echo "Module: $module";

if [[ -d "$year/Day$day" ]]; then
  echo "no"
  exit 2
fi

mkdir -p $year/Day$day
touch $year/data/day$day.txt
touch $year/data/day$day-test.txt
cat >$year/Day$day/"$title".hs <<EOL
module ${module} where

main :: IO ()
main = do
    xs <- readFile "${year}/data/day${day}-test.txt"
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

insert_at_hook aoc2021.cabal "-- !!aoc${year} module hook!! --" ", ${module}"
insert_at_hook $year/Main.hs "-- !!aoc${year} import hook!! --" "import qualified ${module}"
insert_at_hook $year/Main.hs "-- !!aoc${year} main hook!! --" ", ${module}.main"
