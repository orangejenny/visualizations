# This is for examining meta questions like how many responses there are for a particular question.
# It's in a separate file because it's relatively slow to input the data with pyreadstat

import pandas as pd
import pyreadstat

import utils


# Read data
screened_sample, screened_meta = pyreadstat.read_sav(f"{utils.WORKING_DIRECTORY}/asher_data/Dissertation Kathryn Asher (Weighted Cleaned Sample).sav")
screened_columns = pd.DataFrame.from_dict(screened_meta.column_names_to_labels, orient="index")
screened_columns = screened_columns.reset_index()  # turn index into a column

# Get counts of responses per question
key_counts = {}
for key in screened_meta.column_names_to_labels.keys():
    if key == 'ID':
        continue
    key_counts[key] = screened_sample.groupby(key, observed=True).count()['ID'].sum()   # groupby filters out nan, which np.isnan won't do for non-numeric columns
sorted_counts = sorted([(v, k) for k, v in key_counts.items()])
sorted_counts.reverse()

# Each of these categories only has 250-300 responses, which isn't enough for state-level analysis. Same for the thermometer questions.
key_question_map = {
    'MOTIVATIONS': [f"_{x}" for x in ['ANIMAL', 'COST', 'DISGUST', 'ENVIRO', 'HEALTH', 'JUSTICE', 'RELIGION', 'SOCIAL', 'TASTE', 'TREND']],
    'ATTITUDES': [str(i) for i in range(1,5)],
    'BARRIERS': [f"_{x}" for x in ['COST', 'FOODSATISFACTION', 'HEALTH', 'IDENTITY', 'INCONVENIENCE', 'MOTIVATION', 'SOCIALISSUES']],
    'PBC': [str(i) for i in range(1,4)],
}
for category, options in key_question_map.items():
    print(f"Question counts for {category}:")
    for omni in ["", "o"]:
        for diet in ["v", "c", "r"]:
            for option in options:
                key = "".join([omni, diet, category, option])
                print(f"\t{key} => {key_counts[key]}")


'''
indicator_categories = ["FAMILIAR", "TIES", "PERCEPTIONS", "ATTITUDES", "THERMOMETER", "BARRIERS", "MOTIVATIONS", "SWFL"]
for category in indicator_categories:
    keys = [k for k in screened_meta.column_names_to_labels.keys() if category in k]
    print(f"\n{len(keys)} questions for {category}:")
    for k, v in screened_meta.column_names_to_labels.items():
        if category in k:
            count = screened_sample.groupby(k, observed=True).count()['ID'].sum()   # observed=True due to warning
            print(f"{count} responses to: {v}")
'''
