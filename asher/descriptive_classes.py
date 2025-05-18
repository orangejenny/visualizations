'''
This creates an HTML table of descriptive statistics of the flexitarian sample and the classes within it.
'''
import pandas as pd
from statsmodels.stats.weightstats import DescrStatsW

from utils import (
    add_income_continuous,
    add_race_dummy,
    counts_dict,
    proportions,
)

ALL = -1
FAINT = 0
FLOURISHING = 1
FLOUNDERING = 2

do_weight = True


# Load data
data = pd.read_csv("reducers_3_classes_with_pred_weighted.csv")

# Counts of people per class
class_counts = counts_dict(data, 'pred')
print(class_counts)
n_all = sum(class_counts.values())

# Categorical demographics
def proportions_per_class(df, attr, weight=False):
    result = {
        ALL: proportions(df, attr, weight=weight),
    }
    for class_id in [FAINT, FLOURISHING, FLOUNDERING]:
        sub_df = df.loc[df['pred'] == class_id,:].copy()
        result[class_id] = proportions(sub_df, attr, weight=weight)
    return result

data = add_race_dummy(data, 'RACEETHNICITY', 'RACEETHNICITY_dummy')

categoricals = {}
for attr in ['SEX', 'EDUCATION', 'region4', 'RACEETHNICITY_dummy']:
    categoricals[attr] = proportions_per_class(data, attr, weight=do_weight)

data['Age by sex'] = data['SEX'] + ' ' + data['Age_Group']
for attr in ['Age by sex', 'INCOME']:
    categoricals[attr] = proportions_per_class(data, attr, weight=do_weight)

# Continuous demographics
data = add_income_continuous(data)
continuous = {}
for attr in ['AGE', 'INCOME_cont',]:
    continuous[attr] = {}
    for key, df in (
        [(ALL, data)] +
        [(class_id, data.loc[data['pred'] == class_id,:]) for class_id in [FAINT, FLOURISHING, FLOUNDERING]]
    ):
        weights = df['Wts'] if do_weight else None
        stats = DescrStatsW(df[attr], weights=weights)
        continuous[attr][key] = {
            'mean': round(stats.mean) if attr == 'INCOME_cont' else round(stats.mean, 1),
            'std': round(stats.std) if attr == 'INCOME_cont' else round(stats.std, 1),
        }

html = f'''
    <table style="text-align:center;">
        <!-- Class labels -->
        <tr>
            <td></td>
            <td>Full Sample<br>(n={n_all})</td>
            <td>Successful<br>(n={class_counts[FLOURISHING]})</td>
            <td>Superficial<br>(n={class_counts[FAINT]})</td>
            <td>Struggling<br>(n={class_counts[FLOUNDERING]})</td>
        </tr>
        <!-- Demographic rows -->
'''

for (display_label, attr, key) in [
    ("Gender: Men", "SEX", "Male"),
    ("Women 18 to 24", "Age by sex", "Female 18 to 24"),
    ("Women 25 to 34", "Age by sex", "Female 25 to 34"),
    ("Women 35 to 44", "Age by sex", "Female 35 to 44"),
    ("Women 45 to 54", "Age by sex", "Female 45 to 54"),
    ("Women 55 to 64", "Age by sex", "Female 55 to 64"),
    ("Women 65+", "Age by sex", "Female 65+"),
    ("Men 18 to 24", "Age by sex", "Male 18 to 24"),
    ("Men 25 to 34", "Age by sex", "Male 25 to 34"),
    ("Men 35 to 44", "Age by sex", "Male 35 to 44"),
    ("Men 45 to 54", "Age by sex", "Male 45 to 54"),
    ("Men 55 to 64", "Age by sex", "Male 55 to 64"),
    ("Men 65+", "Age by sex", "Male 65+"),
    ("Race: Non-Hispanic White", "RACEETHNICITY_dummy", 0),
    ("Education: Less than high school diploma", "EDUCATION", "Less than 12th grade, no diploma"),
    ("Education: High school diploma", "EDUCATION", "High school diploma (or equivalent)"),
    ("Education: Some college", "EDUCATION", "Some education after high school, no degree"),
    ("Education: College degree", "EDUCATION", "College degree (associate, bachelor’s, master’s, or doctorate)"),
    ("Region: Midwest", "region4", "Midwest"),
    ("Region: Northeast", "region4", "Northeast"),
    ("Region: South", "region4", "South"),
    ("Region: West", "region4", "West"),
    ("Income: $14,999 or less", "INCOME", "$14,999 or less"),
    ("Income: $15,000 to $24,999", "INCOME", "$15,000 to $24,999"),
    ("Income: $25,000 to $34,999", "INCOME", "$25,000 to $34,999"),
    ("Income: $35,000 to $49,999", "INCOME", "$35,000 to $49,999"),
    ("Income: $50,000 to $74,999", "INCOME", "$50,000 to $74,999"),
    ("Income: $75,000 to $99,999", "INCOME", "$75,000 to $99,999"),
    ("Income: $100,000 or over", "INCOME", "$100,000 or over"),
]:
    html += "<tr>"
    html += f"""<td style="text-align: left;">{display_label}</td>"""
    for class_id in [ALL, FLOURISHING, FAINT, FLOUNDERING]:
        html += f"""<td>{categoricals[attr][class_id][key]}%</td>"""
    html += "</tr>"

html += f'''
        <tr>
            <td style="text-align: left;">Age</td>
            <td>{continuous['AGE'][ALL]['mean']} ({continuous['AGE'][ALL]['std']})</td>
            <td>{continuous['AGE'][FLOURISHING]['mean']} ({continuous['AGE'][FLOURISHING]['std']})</td>
            <td>{continuous['AGE'][FAINT]['mean']} ({continuous['AGE'][FAINT]['std']})</td>
            <td>{continuous['AGE'][FLOUNDERING]['mean']} ({continuous['AGE'][FLOUNDERING]['std']})</td>
        </tr>
        <tr>
            <td style="text-align: left;">Income</td>
            <td>${continuous['INCOME_cont'][ALL]['mean']:,} (${continuous['INCOME_cont'][ALL]['std']:,})</td>
            <td>${continuous['INCOME_cont'][FLOURISHING]['mean']:,} (${continuous['INCOME_cont'][FLOURISHING]['std']:,})</td>
            <td>${continuous['INCOME_cont'][FAINT]['mean']:,} (${continuous['INCOME_cont'][FAINT]['std']:,})</td>
            <td>${continuous['INCOME_cont'][FLOUNDERING]['mean']:,} (${continuous['INCOME_cont'][FLOUNDERING]['std']:,})</td>
        </tr>
    </table>
'''

filename = "output/descriptive_classes.html"
with open(filename, "w") as fh:
    print(f"Writing {filename}")
    fh.write(html)
