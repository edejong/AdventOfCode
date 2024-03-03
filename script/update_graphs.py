#! /usr/bin/env python3

from pathlib import Path
import json
import re


def get_stat_files():
    result = []
    # for f in sorted(Path('.').glob('./*/*/aoc[0-9][0-9][0-9][0-9]-[0-9][0-9].stats'), reverse=True):
    for hsFile in sorted(Path('.').glob('./[0-9][0-9][0-9][0-9]/Day[0-9][0-9]/*.hs'), reverse=True):
        # print(f)
        year  = hsFile.parts[0]
        day   = hsFile.parts[1][3:]
        name  = hsFile.stem

        statsFile = hsFile.with_name(f'aoc{year}-{day}.stats')
        
        stats = { 'year': year, 'day': day, 'name': name } | read_stats(statsFile)
        result.append(stats)

    # print(result)
    # exit(1)
    return result


STATS_TUPLE_PATTERN = re.compile(r'\("([\w ]+)", "([^"]+)"\)')


def read_stats(p: Path):
    if not p.exists():
        return {}

    with p.open() as f:
        f.readline()
        contents = f.read()

    matches = STATS_TUPLE_PATTERN.findall(contents)
    return dict(matches)


rows = get_stat_files()
# print(rows)

keys = sorted(list({k for row in rows for k in row.keys()}))
datasets = {}
for k in keys:
    datasets[k] = [row.get(k, None) for row in rows]

with open('stats/stats.js', 'w') as f:
    f.write('const stats = [\n  ')
    # f.write(json.dumps(keys))
    rows_json = [keys] + [[row.get(key, None) for key in keys] for row in rows]
    rows_json = map(json.dumps, rows_json)
    f.write(',\n  '.join(rows_json) + '\n')

    f.write('];\n')
    # json.dump(datasets, f, indent=2)

# datasets = json.dumps(datasets)
# print(datasets)

# print(read_stats(file_path))
