#!/bin/bash

echo "data = ["

# TODO: Go over the directories, not just the ones with .prof files
# for f in $(find -E . -regex './[0-9]{4}/Day[0-9]{2}/aoc[0-9]{4}-[0-9]{2}.*\.prof' | sort );
for d in $(find -E -d . -regex './[0-9]{4}/Day[0-9]{2}' | sort );
do
    regex="([0-9]{4})/Day([0-9]{2})"
    if ! [[ $d =~ $regex ]]
    then
        echo "$d doesn't match" >&2
        continue
    fi

    year="${BASH_REMATCH[1]}"
    day="${BASH_REMATCH[2]}"
    proj_name="aoc$year-$day"

    # Get name from Haskell file
    hs_main=$(grep -l -r -n -i --include='*.hs' 'main =' $d)
    name=$(basename "$hs_main" .hs)

    # Extract data from .prof file
    prof_file="$d/$proj_name.prof"
    if [ -f $prof_file ]; then
        IFS=$' '
        arr=($(cat $prof_file | grep "total time" | tr -d '(),' ))
        total_time_secs=${arr[3]}
        tick_count=${arr[5]}
        tick_duration_us=${arr[8]}
        num_processors=${arr[10]}

        arr=($(cat $prof_file | grep "total alloc" | tr -d '(),' ))
        total_alloc_bytes=${arr[3]}
    else
        unset total_time_secs
        unset tick_count
        unset tick_duration_us
        unset num_processors
        unset total_alloc_bytes
    fi

    echo "  { \"year\": \"$year\", \"day\": \"$day\", \"name\": \"$name\", \"total_time_secs\": \"$total_time_secs\", \"tick_count\": \"$tick_count\", \"tick_duration_us\": \"$tick_duration_us\", \"num_processors\": \"$num_processors\", \"total_alloc_bytes\": \"$total_alloc_bytes\" },"
done
echo "]"

unset IFS
