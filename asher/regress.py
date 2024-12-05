import numpy as np
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf

from plotnine import *
from scipy.stats import zscore
from stargazer.stargazer import Stargazer

import utils

data = pd.read_csv("reducers_3_classes_with_pred.csv")
prefix = 'r'
num_classes = 3

#data = pd.read_csv("non-reducing_3_classes_with_pred.csv")
#prefix = 'ov'

# Re-iterate class prevalences
def _counts(df, label):
    return df.loc[:, ['ID', label]].groupby(label).count()

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
    overall_motivations[key] = percent
    print(f"{prefix}{key} (n={yes + no}) {percent}%")
    data = _recode_by_dict(data, f'r{key}', key, {'Yes': 1, 'No': 0})

# Recode race and white and non-white, due to sample size
race_values = {
    'Other race/ethnicity (including two or more)': 1,
    'Asian': 1,
    'African American or Black': 1,
    'White': 0,
}
data = _recode_by_dict(data, 'RACE', 'RACE_dummy', race_values)

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
sex_data = data.loc[:,['SEX', 'ID', 'pred']].groupby(['pred', 'SEX']).count().to_dict()['ID']
demographic_means = data.loc[:,['pred', 'AGE', 'INCOME_cont', 'EDUCATION_cont']].groupby(['pred']).mean().to_dict()
demographic_medians = data.loc[:,['pred', 'AGE', 'INCOME_cont', 'EDUCATION_cont']].groupby(['pred']).median().to_dict()
race_data = data.loc[:,['pred', 'RACE_dummy', 'ID']].groupby(['pred', 'RACE_dummy']).count().to_dict()['ID']
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
demographic_data = pd.DataFrame.from_records(demographic_records)
print(demographic_data)
# This is hard to interpret because the variables are on different scales
# The interesting ones are gender, age, and maybe income
demographic_plot = (
    ggplot(demographic_data, aes("demographic", "value", fill="factor(pred)"))
    + geom_point()
)
#demographic_plot.show()

# Average servings of meat per day: 2.5, 1, 3 (unweighted)
print(data.loc[:,['pred', 'MEATDAILY']].groupby('pred').mean())

# Limit to relevant analytic set
data = data.loc[np.logical_or(data[f'{prefix}MOTIVATIONS_ANIMAL'] == 'No', data[f'{prefix}MOTIVATIONS_ANIMAL'] == 'Yes'),:]

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
    + labs(x = "", y = "", title = "Correlations between motivations")
)
#matrix.show()

def _add_regression(df, formula, score_label):
    glm_kwargs = {
        'family': sm.families.Binomial(),
        'data': df,
        #'freq_weights': (df['weight'] if do_weight else None),
    }
    model = smf.glm(formula=formula, **glm_kwargs).fit()
    #print(model.summary())
    df[score_label] = model.predict(df)
    return model


# Logistic regressions
class_motivations = {}
for key in utils.MOTIVATION_KEYS:
    formula = f"{key} ~ C(pred)"
    key = key[12:]
    score_key = f'score_{key[12:]}'

    models = []
    model_names = []

    models.append(_add_regression(data, formula, f'{score_key}_nc'))
    model_names.append('No controls')
    
    motivations = data.loc[:, ['pred', f'{score_key}_nc']].groupby(['pred']).min().to_dict()[f'{score_key}_nc']
    motivations = {key: round(value * 100) for key, value in motivations.items()}
    class_motivations[key] = motivations

    formula += " + C(SEX) + AGE + EDUCATION_cont + INCOME_cont"
    models.append(_add_regression(data, formula, f'{score_key}_wc'))
    model_names.append('No race')

    formula += " + C(region4)"
    models.append(_add_regression(data, formula, f'{score_key}_r4'))
    model_names.append('Regions')

    formula = formula.replace("region4", "region9")
    models.append(_add_regression(data, formula, f'{score_key}_r9'))
    model_names.append('Subregions')

    formula += " + C(RACE_dummy)"   # separate and last because it drops 40 observations (14%)
    models.append(_add_regression(data, formula, f'{score_key}_wc'))
    model_names.append('All demographics')

    stargazer = Stargazer(models)
    stargazer.custom_columns(model_names)

    filename = f"stargazers/{key.lower()}.html"
    with open(filename, "w") as fh:
        fh.write(stargazer.render_html())



# Visualize column chart of motivations by class
class_names = ["Alpha3", "Beta3", "Gamma3"]
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
