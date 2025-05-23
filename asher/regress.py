import numpy as np
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf

from collections import defaultdict
from plotnine import *
from scipy.stats import zscore
from stargazer.stargazer import Stargazer

from utils import (
    add_income_continuous,
    add_race_dummy,
    BARRIER_KEYS,
    COMPARISON_OPTIONS,
    CONSUMPTION_OPTIONS,
    COVARIATE_DISPLAY_NAMES,
    convert_categorical_to_numeric,
    counts_table,
    display_barrier,
    display_motivation,
    display_other,
    EXTENT_OPTIONS,
    GENERIC_OPTIONS,
    MOTIVATION_KEYS,
    recode_by_dict,
    WILLINGNESS_OPTIONS,
)


def _write_file(filename, content):
    with open(filename, "w") as fh:
        print(f"Writing {filename}")
        fh.write(content)


data = pd.read_csv("reducers_3_classes_with_pred_weighted.csv")
prefix = 'r'
num_classes = 3
do_weight = True

#data = pd.read_csv("non-reducing_3_classes_with_pred.csv")
#prefix = 'ov'

# Histogram of daily meat consumption
data['MEATSUM'] = data['BEEFDAILY'] + data['PORKDAILY'] + data['CHICKENDAILY'] + data['TURKEYDAILY'] + data['FISHDAILY'] + data['SHELLFISHDAILY'] + data['OTHERMEATSDAILY']
weight_label = "Weighted" if do_weight else "Unweighted"
weight_kwargs = {"weight": data['Wts']} if do_weight else {}
hist = (
    ggplot(data, aes(x = 'MEATSUM', **weight_kwargs))
        + geom_histogram()
        + labs(x = "", y = "", title = f"{weight_label} MEATSUM")
)
#hist.show()

# Print out regional counts
print(counts_table(data, 'region9'))
print(counts_table(data, 'region4'))

# Recode motivations as 0/1
overall_motivations = {}
for key in MOTIVATION_KEYS:
    (no, yes) = counts_table(data, f'{prefix}{key}')['ID']
    percent = round((yes * 100) / (yes + no))
    overall_motivations[key[12:]] = percent
    print(f"{prefix}{key} (n={yes + no}) {percent}%")
    data = recode_by_dict(data, f'r{key}', key, {'Yes': 1, 'No': 0})

# Recode PASTVEG as 0/1
key = 'PASTVEG'
data = recode_by_dict(data, f'r{key}', key, {'Yes': 1, 'No': 0})

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
data = add_race_dummy(data, 'RACE', 'RACE_dummy')
data = add_race_dummy(data, 'RACEETHNICITY', 'RACEETHNICITY_dummy')

# Recode demographics
education_values = {
    'Less than 12th grade, no diploma': 0,
    'High school diploma (or equivalent)': 1,
    'Some education after high school, no degree': 2,
    'College degree (associate, bachelor’s, master’s, or doctorate)': 3,
}
data = recode_by_dict(data, 'EDUCATION', 'EDUCATION_cont', education_values)
data = add_income_continuous(data)

# Add zscores for age and income
data['AGE_zscore'] = zscore(data['AGE'], nan_policy='omit')
data['INCOME_zscore'] = zscore(data['INCOME_cont'], nan_policy='omit')

# Look at demographics by class
sex_data = counts_table(data, ['pred', 'SEX']).to_dict()['ID']
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

# Recode regression outcomes that haven't been recoded yet
data = convert_categorical_to_numeric(data, [f"OPINIONLEADER{i + 1}" for i in range(6)])
data = convert_categorical_to_numeric(data, [f"r{key}" for key in BARRIER_KEYS])
data = convert_categorical_to_numeric(data, [f"rPBC{i + 1}" for i in range(3)], options=GENERIC_OPTIONS)
data = convert_categorical_to_numeric(data, ['rCOMPARISON'], options=COMPARISON_OPTIONS)
data = convert_categorical_to_numeric(data, ['rREDUCEFURTHER', 'rVEGWILLING'], options=WILLINGNESS_OPTIONS)
data = convert_categorical_to_numeric(data, ['rGUILT', 'rINTENTIONS'])
data = convert_categorical_to_numeric(data, ['rTIES'], options=EXTENT_OPTIONS)

# Verify length calculations worked out reasonably
#data.loc[:,['rLENGTH_1_TEXT', 'rLENGTH_2_TEXT', 'rLENGTH_3_TEXT', 'rLENGTH_4_TEXT', 'length_days', 'length_months', 'length_years', 'length_total']]

# Other descriptive aspects of classes, from analytic part of survey
data = convert_categorical_to_numeric(data, [f"SWFL{i + 1}" for i in range(5)], overwrite=False)
swfl_means = data.loc[:,['pred'] + [f'SWFL{i + 1}_numeric' for i in range(5)]].groupby(['pred']).mean()
data.loc[:,['pred', 'rPERCEPTIONS_1']].groupby(['pred']).mean()  # perception of meat reducers in US population
counts_table(data, ['pred', 'rREDUCEFURTHER'])  # willingness to reduce further
counts_table(data, ['pred', 'rVEGWILLING'])     # willingness to go veg
counts_table(data, ['pred', 'rNORMS1'])         # important people think I should eat this way
counts_table(data, ['pred', 'rNORMS2'])
counts_table(data, ['pred', 'rCONFLICTED'])     # conflicted over problems with meat
counts_table(data, ['pred', 'rCOMPARISON'])     # comparison with vegetarianism
data = convert_categorical_to_numeric(data, [f"{prefix}ATTITUDES{i + 1}" for i in range(4)], options=GENERIC_OPTIONS)
attitude_means = data.loc[:,['pred'] + [f'rATTITUDES{i + 1}' for i in range(4)]].groupby(['pred']).mean()
counts_table(data, ['pred', 'rPASTVEG'])        # 60% of 10% class has tried a veg diet in the past, much lower for the others
barrier_means = data.loc[:,['pred'] + [f'{prefix}{k}' for k in BARRIER_KEYS]].groupby(['pred']).mean()    # 10% group struggles more with barriers, they also see diet more as part of their identity

# Calculate total motivations: class 0 selects more motivations
data['MOTIVATION_COUNT'] = data.apply(lambda df: sum([df[k] for k in MOTIVATION_KEYS]), axis=1)
print(data.loc[:,['pred', 'MOTIVATION_COUNT']].groupby('pred').mean())
print(data.loc[:,['pred', 'MOTIVATION_COUNT']].groupby('pred').median())
data.loc[:,['pred', 'MOTIVATION_COUNT', 'Wts']].groupby('pred').sum()

# Correlations among motivations. Strongest corrlations are between ANIMAL, ENVIRO, and JUSTICE
# Visualize as a correlation matrix / heat map
correlation_list = sorted([(round(data[c1].corr(data[c2]), 3), c1, c2) for c1 in MOTIVATION_KEYS for c2 in MOTIVATION_KEYS])
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
    + scale_colour_manual(values=['#ffffff'] * len(MOTIVATION_KEYS))
    + labs(x = "", y = "", title = "Figure 1: Correlations between motivations")
)
#matrix.show()
#matrix.save(filename=f"motivation_correlations.png")


### Visualization (column chart) of motivations
# Add motivation combinations
combination_motivations = []
'''
for key in [
    # Use these as the golden definitions, the variants below don't vary meaningfully
    'COST+DISGUST+HEALTH+SOCIAL+TASTE+TREND:MOTIVATIONS_INTERNAL',
    'ANIMAL+ENVIRO+JUSTICE+RELIGION:MOTIVATIONS_EXTERNAL',

    # Try out different placements of religion
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
'''

class_motivations = defaultdict(dict)
for key in MOTIVATION_KEYS + combination_motivations:
    if do_weight:
        motivations = data.loc[:, ['pred', key, 'Wts']].groupby(['pred', key]).sum().to_dict()['Wts']
    else:
        motivations = data.loc[:, ['pred', key, 'ID']].groupby(['pred', key]).count().to_dict()['ID']
    for class_index in set(data['pred']):
        (no, yes) = (motivations[(class_index, 0)], motivations[(class_index, 1)])
        class_motivations[key][class_index] = round((yes * 100) / (yes + no))

class_names = ["Superficial", "Successful", "Struggling"]
motivation_plot_data = pd.DataFrame.from_records([
    #{'motivation': key, 'pred': 'all', 'prop': value} for key, value in overall_motivations.items()
] + [
    {'motivation': key, 'pred': class_names[class_num], 'prop': class_prop}
    for key, values in class_motivations.items() for class_num, class_prop in values.items()
])

# Print tables of ranked motivations
#motivation_plot_data.loc[motivation_plot_data['pred'] == 'Successful',:].sort_values('prop', ascending=False)
#motivation_plot_data.loc[motivation_plot_data['pred'] == 'Superficial',:].sort_values('prop', ascending=False)
#motivation_plot_data.loc[motivation_plot_data['pred'] == 'Struggling',:].sort_values('prop', ascending=False)

motivation_plot_data['Class'] = motivation_plot_data['pred']    # for legend label
motivation_plot = (
    ggplot(motivation_plot_data, aes(x = "factor(motivation)", y = "prop", fill = "Class"))
        + geom_col(position = "dodge2")
        + scale_y_continuous(limits = [0, 100])
        + theme(axis_text_x=element_text(rotation = 90))
        + labs(x = "", y = "% of class citing reason", title = "Figure 5: Motivations by class")
)
#motivation_plot.show()


records = [
    #{ 'category': '  ', 'value': 1.4, '': 'Non-flexitarians', 'color': '#AAAAAA' },
    { 'category': '  ', 'value': 1.2, ' ': 'Superficial', 'color': '#2196F3' },
    { 'category': '  ', 'value': 0.5, ' ': 'Successful', 'color': '#4CAF50' },
    { 'category': '  ', 'value': 4.5, ' ': 'Struggling', 'color': '#7A4CC6' },
]
class_colors = {rec[' ']: rec['color'] for rec in records}
serving_data = pd.DataFrame.from_records(records)
plot = (
    ggplot(serving_data, aes(x = "factor(category)", y = "value", fill = " "))
        + geom_col(position = "dodge2")
        + scale_y_continuous(limits = [0, 5])
        + theme(axis_text_x=element_text(rotation = 90))
        + scale_fill_manual(values=class_colors, limits=list(class_colors.keys()))
        + labs(x = "", y = "")  #, title = "Daily servings of meat")
        + theme_classic(base_size=24)
        + geom_hline(yintercept=1.4, color="grey", size=1, linetype="dashed")
        + annotate("text", x=1.2, y=1.6, label="Non-flexitarians", size=15)
        #+ coord_flip()
)
plot.show()


# Diet length: successful have the highest median and mean, but this isn't significant
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
demographics = ["C(SEX)", "AGE_zscore", "EDUCATION_cont", "INCOME_zscore", "C(RACEETHNICITY_dummy)"]
region = "C(region4)"
subregion = "C(region9)"
model_specs = [
    ("No controls", "", []),
    ("Demographics", "no_region", demographics),
    ("Demographics<br>+ region", "region", demographics + [region]),
    ("Demographics<br>+ subregion", "subregion", demographics + [subregion]),
]

def write_stargazer(regress, outcome):
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
        #'C(region4)[T.Northeast]',
        #'C(region4)[T.South]',
        #'C(region4)[T.West]',
        'C(RACEETHNICITY_dummy)[T.1]',
    ])
    stargazer.rename_covariates(COVARIATE_DISPLAY_NAMES)

    weight_suffix = "_weighted" if do_weight else ""
    filename = f"stargazers{weight_suffix}/{outcome.lower()}.html"
    _write_file(filename, stargazer.render_html())

    return models[1]    # return details for demographics model


def write_logistic(outcome):
    return write_stargazer(_add_logistic_regression, outcome)


def write_linear(outcome):
    return write_stargazer(_add_linear_regression, outcome)


def write_logistic_coefficient_table(outcomes, filename, _format_outcome=None, display_controls=False):
    return write_coefficient_table(
        [(outcome, _add_logistic_regression) for outcome in outcomes],
        filename, _format_outcome=_format_outcome, display_controls=display_controls
    )


def write_linear_coefficient_table(outcomes, filename, _format_outcome=None, display_controls=False):
    return write_coefficient_table(
        [(outcome, _add_linear_regression) for outcome in outcomes],
        filename, _format_outcome=_format_outcome, display_controls=display_controls
    )


def write_coefficient_table(outcomes_and_adders, filename, _format_outcome=None, display_controls=False):
    if _format_outcome is None:
        _format_outcome = lambda x: x

    controls_label = "_controls" if display_controls else ""
    filename = f"output/cofficients_{filename}{controls_label}.html"
    html = ""

    if display_controls:
        models = {
            outcome: write_stargazer(adder, outcome)
            for (outcome, adder) in outcomes_and_adders
        }
        a_model = models[outcomes_and_adders[0][0]]
        colspan = len(a_model.params)
        html += "<tr><td>&nbsp;</td>"
        for key in a_model.params.keys():
            if key != "Intercept":
                border = "border-right: 1px solid black;" if key == "C(pred)[T.2]" else ""
                html += f'''
                    <td style="vertical-align: top; padding: 0 5px;{border}">
                        {COVARIATE_DISPLAY_NAMES.get(key, key).replace(" (", "<br>(")}
                    </td>
                '''
        html += "</tr>"
        html += f'''
            <tr><td colspan="{colspan}" style="border-bottom: 1px solid black"></td></tr>
        '''
        for (outcome, model) in models.items():
            html += f'''
                <tr><td style="text-align:left">{_format_outcome(outcome)}</td>
            '''
            for key, value in model.params.to_dict().items():
                if key != "Intercept":
                    border = "border-right: 1px solid black;" if key == "C(pred)[T.2]" else ""
                    coeff = round(model.params.to_dict()[key], 2)
                    error = round(model.bse.to_dict()[key], 2)
                    pvalue = model.pvalues.to_dict()[key]
                    stars = get_stars(pvalue)
                    html += f'''
                        <td style="{border}">
                            {coeff}{stars}<br>
                            ({error})
                            <br>({round(pvalue, 20)})
                        </td>
                    '''
            html += "</tr>"
    else:
        colspan = 3
        for (outcome, adder) in outcomes_and_adders:
            model = write_stargazer(adder, outcome)
            html += "<tr>"
            html += f'''<td style="text-align: left;">{_format_outcome(outcome)}</td>'''
            for class_id in [1, 2]:
                key = f'C(pred)[T.{class_id}]'
                coeff = round(model.params.to_dict()[key], 2)
                error = round(model.bse.to_dict()[key], 2)
                pvalue = model.pvalues.to_dict()[key]
                stars = get_stars(pvalue)

                html += f"<td>{coeff}{stars}<br>({error})</td>"
            html += "</tr>"
            html = f'''
                <tr>
                    <td></td>
                    <td>Successful</td>
                    <td>Struggling</td>
                </tr>
                {html}
            '''

    html += f'''
        <tr><td colspan="{colspan}" style="border-bottom: 1px solid black"></td></tr>
        <tr><td colspan="{colspan}" style="text-align: right"><sup>*</sup>p&lt;0.05; <sup>**</sup>p&lt;0.01; <sup>***</sup>p&lt;0.001</td></tr>
    '''
    _write_file(filename, f'''<table style="text-align:center;">{html}</table>''')

    return html


def get_stars(pvalue):
    stars = ''
    if pvalue < 0.001:
        stars = '***'
    elif pvalue < 0.01:
        stars = '**'
    elif pvalue < 0.05:
        stars = '*'
    return stars


'''
# Run regressions
internal_motivation_html = write_logistic_coefficient_table([
    'MOTIVATIONS_COST',
    'MOTIVATIONS_DISGUST',
    'MOTIVATIONS_HEALTH',
    'MOTIVATIONS_SOCIAL',
    'MOTIVATIONS_TASTE',
    'MOTIVATIONS_TREND',
    #'MOTIVATIONS_INTERNAL',
], 'motivations_internal', display_motivation)

external_motivation_html = write_logistic_coefficient_table([
    'MOTIVATIONS_ANIMAL',
    'MOTIVATIONS_ENVIRO',
    'MOTIVATIONS_JUSTICE',
    'MOTIVATIONS_RELIGION',
    #'MOTIVATIONS_EXTERNAL',
], 'motivations_external', display_motivation)

write_logistic_coefficient_table(combination_motivations, 'combined_motivations', display_motivation)
'''
write_logistic_coefficient_table(MOTIVATION_KEYS, 'motivations', display_motivation, display_controls=True)
exit(0)

barrier_html = write_linear_coefficient_table([
    'rBARRIERS_COST',
    'rBARRIERS_INCONVENIENCE',
    'rBARRIERS_MOTIVATION',
    'rBARRIERS_SOCIALISSUES',
], 'barriers', display_barrier, display_controls=True)

facilitator_html = write_linear_coefficient_table([
    'rBARRIERS_FOODSATISFACTION',
    'rBARRIERS_HEALTH',
    'rBARRIERS_IDENTITY',
    'rTIES',
], 'facilitators', display_barrier, display_controls=True)

past_html = write_coefficient_table([
    ("length_total", _add_linear_regression),
    ("PASTVEG", _add_logistic_regression),
], 'past', display_other)

future_html = write_coefficient_table([
    ("rINTENTIONS", _add_linear_regression),
    ("rREDUCEFURTHER", _add_linear_regression),
    ("rVEGWILLING", _add_linear_regression),
], 'future', display_other)



# Write one giant table for paper
filename = "output/coefficients_all.html"
_write_file(filename, f"""
    <table style="text-align:center;">
        <tr>
        <td style="vertical-align: top;">
        <table>
        <tr>
            <td></td>
            <td style="font-weight: bold;">Successful</td>
            <td style="font-weight: bold;">Struggling</td>
        </tr>
        <tr><td style="font-weight: bold; text-align: left;">Motivations (internal)</td></tr>
        {internal_motivation_html}
        <tr><td style="font-weight: bold; text-align: left;">Motivations (external)</td></tr>
        {external_motivation_html}
        </table>
        </td>
        <td style="vertical-align: top;">
        <table>
        <tr>
            <td></td>
            <td style="font-weight: bold;">Successful</td>
            <td style="font-weight: bold;">Struggling</td>
        </tr>
        <tr><td style="font-weight: bold; text-align: left;">Barriers</td></tr>
        {barrier_html}
        <tr><td style="font-weight: bold; text-align: left;">Facilitators</td></tr>
        {facilitator_html}
        <tr><td style="font-weight: bold; text-align: left;">Past behavior</td></tr>
        {past_html}
        <tr><td style="font-weight: bold; text-align: left;">Future intentions</td></tr>
        {future_html}
        </table>
        </td>
        </tr>
    </table>
""")

# A few more regressions
write_logistic('PASTVEG')

for i in range(3):
    write_linear(f'rPBC{i + 1}')

for i in range(6):
    write_linear(f'OPINIONLEADER{i + 1}')

for i in range(4):
    write_linear(f'rATTITUDES{i + 1}')

write_linear('rPERCEPTIONS_1')
write_linear('length_total')
write_linear('MOTIVATION_COUNT')
write_linear('rCOMPARISON')
write_linear('rGUILT')
write_linear('rINTENTIONS')
write_linear('rREDUCEFURTHER')
write_linear('rVEGWILLING')
write_linear('rTIES')
