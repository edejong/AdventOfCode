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
main :: IO ()
main = do
    xs <- readFile "${srcdir}/day${day}-test.txt"
    print xs
EOL

read -r -d '' VAR <<- EOM
executable aoc${year}-${day}
    main-is:          ${title}.hs
    other-modules:
          Paths_adventOfCode
    build-depends:
          base >=4.17 && <5
    hs-source-dirs:   ${srcdir}
    ghc-options:      -threaded -rtsopts -with-rtsopts=-N
    default-language: Haskell2010
EOM

echo "" >> adventOfCode.cabal
echo "$VAR" >> adventOfCode.cabal
