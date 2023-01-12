import pandas as pd
import os

count_dir = 'counts'

count_folders = os.listdir(count_dir)

files = {f:os.listdir(os.path.join(count_dir, f)) for f in count_folders}

from collections import defaultdict as dd

results = dd(list)

for name, csvs in files.items():
    for csv in csvs:
        position = ' '.join(csv.split(' ')[1:]) if ' ' in csv else 'start'
        d = pd.read_csv(os.path.join(count_dir, name, csv))
        d['tag'] = name

        results[position].append(d)

results = {k:pd.concat(v).fillna(0) for k,v in results.items()}
results = {k:v[['tag'] + [c for c in v.columns if c != 'tag']] for k,v in results.items()}

# make directory for counts by position
if not os.path.exists(os.path.join(count_dir, 'by_position')):
    os.mkdir(os.path.join(count_dir, 'by_position'))

for k,v in results.items():
    # write to csv except first column
    v.to_csv(os.path.join(count_dir, 'by_position', k + '.csv'), index=False)