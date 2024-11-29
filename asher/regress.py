import numpy as np
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf

from plotnine import *

import utils

data = pd.read_csv("reducers_3_classes_with_pred.csv")
prefix = 'r'

#data = pd.read_csv("non-reducing_3_classes_with_pred.csv")
#prefix = 'ov'

# Limit to relevant analytic set
data = data.loc[np.logical_or(data[f'{prefix}MOTIVATIONS_ANIMAL'] == 'No', data[f'{prefix}MOTIVATIONS_ANIMAL'] == 'Yes'),:]

# Re-iterate class prevalences
print(data.loc[:,['ID', 'pred']].groupby('pred').count())

# Print out regional counts
print(data.loc[:,['region9', 'ID']].groupby('region9').count())
print(data.loc[:,['region4', 'ID']].groupby('region4').count())

def _recode_by_dict(df, old_label, new_label, values):
    df[new_label] = data.apply(lambda df: values[df[old_label]], axis=1)
    return df

# Recode motivations as 0/1
overall_motivations = {}
for key in utils.MOTIVATION_KEYS:
    (no, yes) = data.groupby(f'{prefix}{key}').count()['ID']
    percent = round((yes * 100) / (yes + no))
    overall_motivations[key] = percent
    print(f"{prefix}{key} (n={yes + no}) {percent}%")
    data = _recode_by_dict(data, f'r{key}', key, {'Yes': 1, 'No': 0})

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

# Calculate total motivations: class 0 selects more motivations
data['MOTIVATION_COUNT'] = data.apply(lambda df: sum([df[k] for k in utils.MOTIVATION_KEYS]), axis=1)
print(data.loc[:,['pred', 'MOTIVATION_COUNT']].groupby('pred').mean())

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
matrix.show()

# Logistic regressions
class_motivations = {}
for key in utils.MOTIVATION_KEYS:
    score_key = f'score_{key[12:]}'
    glm_kwargs = {
        'family': sm.families.Binomial(),
        'data': data,
        #'freq_weights': (df['weight'] if do_weight else None),
    }
    formula = f"{key} ~ C(pred)"
    logit = smf.glm(formula=formula, **glm_kwargs).fit()
    data[f'{score_key}_nc'] = logit.predict(data)
    
    motivations = data.loc[:, ['pred', f'{score_key}_nc']].groupby(['pred']).min().to_dict()[f'{score_key}_nc']
    motivations = {key: round(value * 100) for key, value in motivations.items()}
    class_motivations[key] = motivations
    #print(logit.summary())

    formula += " + C(SEX) + C(RACE) + AGE + EDUCATION_cont + INCOME_cont"
    logit = smf.glm(formula=formula, **glm_kwargs).fit()
    data[f'{score_key}_wc'] = logit.predict(data)
    #print(logit.summary())

motivation_plot_data = pd.DataFrame.from_records([
    #{'motivation': key[12:], 'pred': 'all', 'prop': value} for key, value in overall_motivations.items()
] + [
    {'motivation': key[12:], 'pred': class_num, 'prop': class_prop}
    for key, values in class_motivations.items() for class_num, class_prop in values.items()
])
motivation_plot = (
    ggplot(motivation_plot_data, aes(x = "factor(motivation)", y = "prop", fill = "factor(pred)"))
        + geom_col(position = "dodge2")
        + scale_y_continuous(limits = [0, 100])
        + theme(axis_text_x=element_text(rotation = 90))
        + labs(x = "", y = "", title = "Motivations")
)
motivation_plot.show()

'''
TODO
    - Set up model summaries with stargazer: https://github.com/StatsReporting/stargazer
    - Make more models with fixed effects for region: C(region9) and C(region4)
    - Learn to interpret models
'''
