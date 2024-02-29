import os
import pandas as pd

from datetime import datetime


OUTPUT_DIR = 'output'
OUTPUT_FILES = ['significant.log', 'all.log', 'two_stars.log', 'three_stars.log']

def truncate_output():
    for filename in OUTPUT_FILES:
        with open(os.path.join(OUTPUT_DIR, filename), 'w') as fh:
            fh.write(f"Run started {datetime.now()}\n")

def _output(filename, data, description=''):
    if type(data) == pd.DataFrame:
        data = data.to_string(max_rows=100)
    else:
        data = str(data)
    if description:
        data = "\n" + description + "\n" + data
    with open(os.path.join(OUTPUT_DIR, filename), 'a') as fh:
        fh.write(data + "\n")

def log_verbose(data, description=''):
    _output('all.log', data, description)

def log_header(header):
    print(header)
    for filename in OUTPUT_FILES:
        _output(filename, header)

def log_findings(data, description=''):
    _output('all.log', data, description)
    _output('significant.log', _limit_to_significant(data), description)
    _output('two_stars.log', _limit_to_significant(data, level=2), description)
    _output('three_stars.log', _limit_to_significant(data, level=3), description)

def _limit_to_significant(data, level=1):
    key = '*' * level
    if 'pvalue' in data:
        data = data.astype({'pvalue': pd.StringDtype()})
        data['sig'] = data['pvalue'].str.find(key) != -1
    else:
        for col in data.columns:
            if col.endswith('*'):
                data[col.replace('*', '?')] = data[col].str.find(key) != -1
        data['sig'] = data.any(axis=1, bool_only=True)
        data.drop([col for col in data.columns if col.endswith("?")], axis=1, inplace=True)
    data = data.loc[data['sig'],:]
    data = data.drop(['sig'], axis=1)
    return data
