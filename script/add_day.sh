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
touch $srcdir/day$day-ex.txt
cat >$srcdir/"$title".hs <<EOL

main :: IO ()
main = do
    xs <- readFile "${srcdir}/day${day}-ex.txt"
    print xs
EOL

read -r -d '' VAR <<- EOM
executable aoc${year}-${day}
    import:           defaults, exec-defaults
    main-is:          ${title}.hs
    hs-source-dirs:   ${srcdir}
--     build-depends:
EOM

echo "" >> adventOfCode.cabal
echo "$VAR" >> adventOfCode.cabal
