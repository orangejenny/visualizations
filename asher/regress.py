import numpy as np
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf

from collections import defaultdict
from plotnine import *
from scipy.stats import zscore
from stargazer.stargazer import Stargazer

import utils

data = pd.read_csv("reducers_3_classes_with_pred_weighted.csv")
prefix = 'r'
num_classes = 3
do_weight = True

#data = pd.read_csv("non-reducing_3_classes_with_pred.csv")
#prefix = 'ov'

# Re-iterate class prevalences
def _counts(df, label):
    if type(label) != list:
        label = [label]
    return df.loc[:, ['ID'] + label].groupby(label).count()

print(_counts(data, 'pred'))

# Print out regional counts
print(_counts(data, 'region9'))
print(_counts(data, 'region4'))

def _recode_by_dict(df, old_label, new_label, values):
    df[new_label] = data.apply(lambda df: values.get(df[old_label], np.nan), axis=1)
    return df

# Recode motivations as 0/1
overall_motivations = {}
for key in utils.MOTIVATION_KEYS:
    (no, yes) = _counts(data, f'{prefix}{key}')['ID']
    percent = round((yes * 100) / (yes + no))
    overall_motivations[key[12:]] = percent
    print(f"{prefix}{key} (n={yes + no}) {percent}%")
    data = _recode_by_dict(data, f'r{key}', key, {'Yes': 1, 'No': 0})

# Recode PASTVEG as 0/1
key = 'PASTVEG'
data = _recode_by_dict(data, f'r{key}', key, {'Yes': 1, 'No': 0})

# Calculate length of time on diet
data['length_days'] = data['rLENGTH_3_TEXT'] / 365.25
data['length_months'] = data['rLENGTH_2_TEXT'] / 12
data['length_years'] = data['rLENGTH_1_TEXT']
data.loc[data['length_years'] > 100, 'length_months'] = 0   # fully exclude any rows where year vlue is nonsensical
data.loc[data['length_years'] > 100, 'length_days'] = 0
data.loc[data['length_years'] > 100, 'length_years'] = 0
data.loc[np.isnan(data['rLENGTH_1_TEXT']), 'length_years'] = 0
data.loc[np.isnan(data['rLENGTH_2_TEXT']), 'length_months'] = 0
data.loc[np.isnan(data['rLENGTH_3_TEXT']), 'length_days'] = 0
data['length_total'] = data.apply(lambda df: df['length_years'] + df['length_months'] + df['length_days'], axis=1)
data.loc[data['length_total'] == 0, 'length_total'] = np.nan

# Recode race and non-Hispanic white and non-white, due to sample size
race_values = {
    'Other race/ethnicity (including two or more)': 1,
    'Asian': 1,
    'African American or Black': 1,
    'HIspanic': 1,
    'White': 0,
}
data = _recode_by_dict(data, 'RACE', 'RACE_dummy', race_values)
data = _recode_by_dict(data, 'RACEETHNICITY', 'RACEETHNICITY_dummy', race_values)

# Recode demographics
education_values = {
    'Less than 12th grade, no diploma': 0,
    'High school diploma (or equivalent)': 1,
    'Some education after high school, no degree': 2,
    'College degree (associate, bachelor’s, master’s, or doctorate)': 3,
}
data = _recode_by_dict(data, 'EDUCATION', 'EDUCATION_cont', education_values)
income_values = {
    '$14,999 or less': 7500,
    '$15,000 to $24,999': 20_000,
    '$25,000 to $34,999': 30_000,
    '$35,000 to $49,999': 42_500,
    '$50,000 to $74,999': 62_500,
    '$75,000 to $99,999': 87_500,
    '$100,000 or over': 150_000,
}
data = _recode_by_dict(data, 'INCOME', 'INCOME_cont', income_values)

# Add zscores for age and income
data['AGE_zscore'] = zscore(data['AGE'], nan_policy='omit')
data['INCOME_zscore'] = zscore(data['INCOME_cont'], nan_policy='omit')

# Look at demographics by class
sex_data = _counts(data, ['pred', 'SEX']).to_dict()['ID']
demographic_means = data.loc[:,['pred', 'AGE', 'INCOME_cont', 'EDUCATION_cont']].groupby(['pred']).mean().to_dict()
demographic_medians = data.loc[:,['pred', 'AGE', 'INCOME_cont', 'EDUCATION_cont']].groupby(['pred']).median().to_dict()
race_data = data.loc[:,['pred', 'RACE_dummy', 'ID']].groupby(['pred', 'RACE_dummy']).count().to_dict()['ID']
raceethnicity_data = data.loc[:,['pred', 'RACEETHNICITY_dummy', 'ID']].groupby(['pred', 'RACEETHNICITY_dummy']).count().to_dict()['ID']
region_data = data.loc[:,['pred', 'region4', 'ID']].groupby(['pred', 'region4']).count().to_dict()['ID']
demographic_records = []
for class_num in range(num_classes):
    sex = {key[1]: value for key, value in sex_data.items() if key[0] == class_num}
    demographic_records.append({
        "demographic": "female",
        "value": sex['Female'] * 100 / sum(sex.values()),
        "pred": class_num,
    })
    race = {key[1]: value for key, value in race_data.items() if key[0] == class_num}
    demographic_records.append({
        "demographic": "white",
        "value": race[0] * 100 / sum(race.values()),
        "pred": class_num,
    })
    region = {key[1]: value for key, value in region_data.items() if key[0] == class_num}
    for region_name, region_count in region.items():
        demographic_records.append({
            "demographic": region_name,
            "value": region_count * 100 / sum(region.values()),
            "pred": class_num,
        })
    for metric in ['AGE', 'INCOME_cont', 'EDUCATION_cont']:
        demographic_records.append({
            "demographic": metric.replace("_cont", "").lower() + "_mean",
            "value": demographic_means[metric][class_num],
            "pred": class_num,
        })
        demographic_records.append({
            "demographic": metric.replace("_cont", "").lower() + "_median",
            "value": demographic_medians[metric][class_num],
            "pred": class_num,
        })
for record in demographic_records:
    if "education" not in record["demographic"]:
        record["value"] = round(record["value"])
demographic_data = pd.DataFrame.from_records(demographic_records)
print(demographic_data)
# This is hard to interpret because the variables are on different scales
# The interesting ones are gender, age, and maybe income
demographic_plot = (
    ggplot(demographic_data, aes("demographic", "value", fill="factor(pred)"))
    + geom_point()
)
#demographic_plot.show()

# Average servings of meat per day, recreating a numeric variable for MEATDAILY, which is categorical
data['newMEATDAILY'] = data.apply(lambda df: df['BEEFDAILY'] + df['PORKDAILY'] + df['CHICKENDAILY'] + df['TURKEYDAILY'] + df['FISHDAILY'] + df['SHELLFISHDAILY'] + df['OTHERMEATSDAILY'], axis=1)
# Unweighted: 1.2, 0.4, 6.4
data.loc[:,['pred', 'newMEATDAILY']].groupby('pred').mean()
# Weighted: 1.2, 0.4, 4.5
data.loc[:,['pred', 'newMEATDAILY', 'Wts']].groupby('pred').sum()

# Limit to relevant analytic set
data = data.loc[np.logical_or(data[f'{prefix}MOTIVATIONS_ANIMAL'] == 'No', data[f'{prefix}MOTIVATIONS_ANIMAL'] == 'Yes'),:]

# Verify length calculations worked out reasonably
#data.loc[:,['rLENGTH_1_TEXT', 'rLENGTH_2_TEXT', 'rLENGTH_3_TEXT', 'rLENGTH_4_TEXT', 'length_days', 'length_months', 'length_years', 'length_total']]

# Other descriptive aspects of classes, from analytic part of survey
data = utils.convert_categorical_to_numeric(data, [f"SWFL{i + 1}" for i in range(5)], overwrite=False)
swfl_means = data.loc[:,['pred'] + [f'SWFL{i + 1}_numeric' for i in range(5)]].groupby(['pred']).mean()
data.loc[:,['pred', 'rPERCEPTIONS_1']].groupby(['pred']).mean()  # perception of meat reducers in US population
_counts(data, ['pred', 'rREDUCEFURTHER'])  # willingness to reduce further
_counts(data, ['pred', 'rVEGWILLING'])     # willingness to go veg
_counts(data, ['pred', 'rNORMS1'])         # important people think I should eat this way
_counts(data, ['pred', 'rNORMS2'])
_counts(data, ['pred', 'rCONFLICTED'])     # conflicted over problems with meat
_counts(data, ['pred', 'rCOMPARISON'])     # comparison with vegetarianism
data = utils.convert_categorical_to_numeric(data, [f"{prefix}ATTITUDES{i + 1}" for i in range(4)], options=utils.GENERIC_OPTIONS)
attitude_means = data.loc[:,['pred'] + [f'rATTITUDES{i + 1}' for i in range(4)]].groupby(['pred']).mean()
_counts(data, ['pred', 'rPASTVEG'])        # 60% of 10% class has tried a veg diet in the past, much lower for the others
data = utils.convert_categorical_to_numeric(data, [f"{prefix}{k}" for k in utils.BARRIER_KEYS])
barrier_means = data.loc[:,['pred'] + [f'{prefix}{k}' for k in utils.BARRIER_KEYS]].groupby(['pred']).mean()    # 10% group struggles more with barriers, they also see diet more as part of their identity

# Calculate total motivations: class 0 selects more motivations
data['MOTIVATION_COUNT'] = data.apply(lambda df: sum([df[k] for k in utils.MOTIVATION_KEYS]), axis=1)
print(data.loc[:,['pred', 'MOTIVATION_COUNT']].groupby('pred').mean())
print(data.loc[:,['pred', 'MOTIVATION_COUNT']].groupby('pred').median())

# Correlations among motivations. Strongest corrlations are between ANIMAL, ENVIRO, and JUSTICE
# Visualize as a correlation matrix / heat map
correlation_list = sorted([(round(data[c1].corr(data[c2]), 3), c1, c2) for c1 in utils.MOTIVATION_KEYS for c2 in utils.MOTIVATION_KEYS])
correlations = pd.DataFrame.from_records([
    {
        'motivation1': m1[12:],
        'motivation2': m2[12:],
        'correlation': value if m1 <= m2 else None,
    }
    for value, m1, m2 in correlation_list
])
matrix = (
    ggplot(correlations, aes("motivation1", "motivation2", fill="correlation"))
    + geom_tile(aes(width=0.95, height=0.95))
    + geom_text(aes(label="correlation", colour='motivation1'), size=9)
    + theme(axis_text_x=element_text(rotation = 90))
    + scale_colour_manual(values=['#ffffff'] * len(utils.MOTIVATION_KEYS))
    + labs(x = "", y = "", title = "Figure 1: Correlations between motivations")
)
#matrix.show()
#matrix.save(filename=f"motivation_correlations.png")


### Visualization (column chart) of motivations
# Add motivation combinations
combination_motivations = []
for key in [
    'COST+DISGUST+HEALTH+SOCIAL+TASTE+TREND:MOTIVATIONS_INTERNAL1',
    'ANIMAL+ENVIRO+JUSTICE+RELIGION:MOTIVATIONS_EXTERNAL1',
    'COST+DISGUST+HEALTH+SOCIAL+TASTE+TREND+RELIGION:MOTIVATIONS_INTERNAL2',
    'ANIMAL+ENVIRO+JUSTICE:MOTIVATIONS_EXTERNAL2',
    'COST+DISGUST+HEALTH+SOCIAL+TASTE+TREND:MOTIVATIONS_INTERNAL3',
    'ANIMAL+ENVIRO+JUSTICE:MOTIVATIONS_EXTERNAL3',
]:
    (inputs, key) = key.split(":")
    data[key] = data.apply(lambda df: min(1, sum([df[f'MOTIVATIONS_{k}'] for k in inputs.split('+')])), axis=1)
    combination_motivations.append(key)

class_motivations = defaultdict(dict)
for key in utils.MOTIVATION_KEYS + combination_motivations:
    motivations = data.loc[:, ['pred', key, 'ID']].groupby(['pred', key]).count().to_dict()['ID']
    for class_index in set(data['pred']):
        (no, yes) = (motivations[(class_index, 0)], motivations[(class_index, 1)])
        class_motivations[key][class_index] = round((yes * 100) / (yes + no))

class_names = ["Faint", "Flourishing", "Floundering"]
motivation_plot_data = pd.DataFrame.from_records([
    #{'motivation': key, 'pred': 'all', 'prop': value} for key, value in overall_motivations.items()
] + [
    {'motivation': key, 'pred': class_names[class_num], 'prop': class_prop}
    for key, values in class_motivations.items() for class_num, class_prop in values.items()
])
motivation_plot_data['Class'] = motivation_plot_data['pred']    # for legend label
motivation_plot = (
    ggplot(motivation_plot_data, aes(x = "factor(motivation)", y = "prop", fill = "Class"))
        + geom_col(position = "dodge2")
        + scale_y_continuous(limits = [0, 100])
        + theme(axis_text_x=element_text(rotation = 90))
        + labs(x = "", y = "% of class citing reason", title = "Figure 5: Motivations by class")
)
#motivation_plot.show()


# Diet length: flourishing have the highest median and mean, but this isn't significant
# It *is* significant for flourishing, if outliers are 20+ years instead of 100+ years
# Data quality is a quesiton, only 200 people reported
lengths = data.loc[data['length_total'] > 0, ['pred', 'length_total']].copy()
lengths.groupby('pred').mean()
lengths.groupby('pred').median()


### Logistic regressions
def _get_model(df, family, outcome, controls=None):
    formula = f"{outcome} ~ C(pred)"
    if controls:
        formula += " + " + " + ".join(controls)

    glm_kwargs = {
        'family': family,
        'data': df,
        'freq_weights': (df['Wts'] if do_weight else None),
    }
    return smf.glm(formula=formula, **glm_kwargs).fit()


def _add_regression(df, family, outcome, controls, suffix=""):
    model = _get_model(df, family, outcome, controls)
    print(model.summary())
    suffix = f"_{suffix}" if suffix else ""
    df[f'score_model{suffix}'] = model.predict(df)
    return model


def _add_logistic_regression(df, outcome, controls=None, suffix=""):
    return _add_regression(df, sm.families.Binomial(), outcome, controls, suffix)


def _add_linear_regression(df, outcome, controls=None, suffix=""):
    return _add_regression(df, sm.families.Gaussian(), outcome, controls, suffix)


# Logistic regressions
for regress, outcome in [
    (_add_logistic_regression, k) for k in utils.MOTIVATION_KEYS + combination_motivations + ['PASTVEG']
] + [
    (_add_linear_regression, k) for k in ['rPERCEPTIONS_1', 'length_total', 'MOTIVATION_COUNT']
]:
    demographics = ["C(SEX)", "AGE_zscore", "EDUCATION_cont", "INCOME_zscore", "C(RACEETHNICITY_dummy)"]
    region = "C(region4)"
    subregion = "C(region9)"
    model_specs = [
        ("No controls", "", []),
        ("Demographics", "no_region", demographics),
        ("Demographics<br>+ region", "region", demographics + [region]),
        ("Demographics<br>+ subregion", "subregion", demographics + [subregion]),
    ]
    models = []
    model_names = []

    for spec in model_specs:
        models.append(regress(data, outcome, spec[2], spec[1]))
        model_names.append(spec[0])

    stargazer = Stargazer(models)
    stargazer.custom_columns(model_names)
    stargazer.show_model_numbers(False)
    stargazer.significant_digits(2)
    stargazer.significance_levels([0.05, 0.01, 0.001])
    stargazer.covariate_order([
        'C(pred)[T.1]',
        'C(pred)[T.2]',
        'AGE_zscore',
        'C(SEX)[T.Male]',
        'INCOME_zscore',
        'EDUCATION_cont',
        'C(region4)[T.Northeast]',
        'C(region4)[T.South]',
        'C(region4)[T.West]',
        'C(RACEETHNICITY_dummy)[T.1]',
    ])
    stargazer.rename_covariates({
        'AGE_zscore': 'Age',
        'C(RACEETHNICITY_dummy)[T.1.0]': 'Race (non-white)',
        'C(SEX)[T.Male]': 'Sex (male)',
        'C(pred)[T.1]': 'Class: Flourishing<br>(20% semi-vegetarians)',
        'C(pred)[T.2]': 'Class: Floundering<br>(10% heavy meat eaters)',
        'C(region4)[T.Northeast]': 'Region (Northeast)',
        'C(region4)[T.South]': 'Region (South)',
        'C(region4)[T.West]': 'Region (West)',
        'EDUCATION_cont': 'Education',
        'INCOME_zscore': 'Income',
    })

    weight_suffix = "_weighted" if do_weight else ""
    filename = f"stargazers{weight_suffix}/{outcome.lower()}.html"
    with open(filename, "w") as fh:
        print(f"Writing {filename}")
        fh.write(stargazer.render_html())
