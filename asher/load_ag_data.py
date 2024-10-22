import pandas as pd

from collections import defaultdict

import utils


def add_state_indicator(states, data, state_col, value_col, value_label, is_upper=False):
    print(f"Adding {value_label}")
    data = data.loc[:,[state_col, value_col]]
    states = pd.merge(states.copy(), data, how='left', right_on=state_col, left_on='state_upper' if is_upper else 'state')
    states.drop(state_col, axis=1, inplace=True)
    states = states.astype({value_col: 'float'})
    states = states.rename(columns={value_col: value_label})
    return states

def add_nass_state_indicator(states, filename, value_label):
    data = pd.read_csv(f"{utils.WORKING_DIRECTORY}/ag_data/{filename}")
    data = data.loc[:, ['State', 'Value']]
    for index, values in data.iterrows():
        data.iloc[index, 1] = data.iloc[index, 1].replace(',', '')
        if (data.iloc[index, 1].strip() == "(D)"):
            data.iloc[index, 1] = 0
    return add_state_indicator(states, data, 'State', 'Value', value_label, is_upper=True)


states = pd.read_csv(f"{utils.WORKING_DIRECTORY}/{utils.STATES_CSV}")

population = pd.read_excel(f"{utils.WORKING_DIRECTORY}/ag_data/NST-EST2023-POP.xlsx")
population = population.iloc[range(8, 59), [0, 4]]   # grab 2022 data
population.rename(columns={population.columns[0]: 'State', population.columns[1]: 'Value'}, inplace=True)
population.State = population.apply(lambda L: L.State.replace('.', ''), axis=1)
states.drop('id', axis=1, inplace=True)
states = add_state_indicator(states, population, 'State', 'Value', 'population')

farms = pd.read_excel(f"{utils.WORKING_DIRECTORY}/ag_data/ag-number-of-farms.xls", sheet_name=1)
farms = farms.iloc[range(2, 55), [1, 21]]   # grab 2012 data, the most recent
states = add_state_indicator(states, farms, 'Unnamed: 1', 'Unnamed: 21', 'farms')

acreage = pd.read_excel(f"{utils.WORKING_DIRECTORY}/ag_data/2024_fsa_acres_web_080924.xlsx") # missing Hawaii
states = add_state_indicator(states, acreage, 'Unnamed: 0', 'Unnamed: 12', 'acres')

states = add_nass_state_indicator(states, "farmers.csv", 'farmers')
states = add_nass_state_indicator(states, "cows.csv", 'cows')
states = add_nass_state_indicator(states, "broilers.csv", 'broilers')
states = add_nass_state_indicator(states, "hogs.csv", 'hogs')
states = add_nass_state_indicator(states, "layers.csv", 'layers')

labor_category = "Farmworkers, Farm, Ranch, and Aquacultural Animals"
labor = pd.read_excel(f"{utils.WORKING_DIRECTORY}/ag_data/state_M2023_dl.xlsx")
labor = labor.loc[labor['OCC_TITLE'] == labor_category, :]
states = add_state_indicator(states, labor, 'AREA_TITLE', 'TOT_EMP', 'employees')

# Top states for each of the agricultural metrics
n = 5
top_states = defaultdict(lambda: 0)
ag_indicators = ['farms', 'farmers', 'acres', 'cows', 'broilers', 'hogs', 'layers', 'employees']
for indicator in ag_indicators:
    top_n = states.sort_values([indicator], ascending=False)[:n].loc[:,['state']]
    for state in top_n['state']:
        top_states[state] += 1

print("Adjusting indicators for population")
for indicator in ag_indicators:
    data = states.loc[:,['state', 'population', indicator]].copy()
    data["per_capita"] = data.apply(lambda df: df[indicator] / df.population if df[indicator] else 0, axis=1)
    top_n = data.sort_values(["per_capita"], ascending=False)[:n].loc[:,['state']]
    for state in top_n['state']:
        top_states[state] += 1

# Animals/acres per resident - as this increases, farming is MORE influential
for indicator in ['acres', 'cows', 'broilers', 'hogs', 'layers']:
    states[f"{indicator}_adjusted"] = states.apply(lambda df: df[indicator] / df.population if df[indicator] else 0, axis=1)

# Residents per farm/farmer/employee - as this increases, farming is LESS influential
for indicator in ['farms', 'farmers', 'employees']:
    states[f"{indicator}_adjusted"] = states.apply(lambda df: df.population / df[indicator] if df[indicator] else 0, axis=1)

print("Ranking states by indicator")
for indicator in ag_indicators:
    states[f"rank_{indicator}"] = states[f"{indicator}"].rank(ascending=False)
    states[f"rank_{indicator}_adjusted"] = states[f"{indicator}_adjusted"].rank(ascending=False)

states.drop('state_upper', axis=1, inplace=True)

# Write CSV
if input(f"Write to {utils.AG_CSV}? ") == "y":
    states.to_csv(f"{utils.WORKING_DIRECTORY}/{utils.AG_CSV}")

print("Done")
