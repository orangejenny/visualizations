'''
This creates an HTML table of descriptive statistics of the flexitarian sample and the classes within it.
'''
import pandas as pd

from utils import (
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

html = f'''
    <table style="text-align:center;">
        <!-- Class labels -->
        <tr>
            <td></td>
            <td>Full Sample<br>(n={n_all})</td>
            <td>Faint<br>(n={class_counts[FAINT]})</td>
            <td>Flourishing<br>(n={class_counts[FLOURISHING]})</td>
            <td>Floundering<br>(n={class_counts[FLOUNDERING]})</td>
        </tr>
        <!-- Demographic rows -->
        <tr>
            <td style="text-align: left;">Gender: Men</td>
            <td>{categoricals['SEX'][ALL]['Male']}%</td>
            <td>{categoricals['SEX'][FAINT]['Male']}%</td>
            <td>{categoricals['SEX'][FLOURISHING]['Male']}%</td>
            <td>{categoricals['SEX'][FLOUNDERING]['Male']}%</td>
        </tr>
        <tr>
            <td style="text-align: left;">Race: Non-Hispanic White</td>
            <td>{categoricals['RACEETHNICITY_dummy'][ALL][0]}%</td>
            <td>{categoricals['RACEETHNICITY_dummy'][FAINT][0]}%</td>
            <td>{categoricals['RACEETHNICITY_dummy'][FLOURISHING][0]}%</td>
            <td>{categoricals['RACEETHNICITY_dummy'][FLOUNDERING][0]}%</td>
        </tr>
        <tr>
            <td style="text-align: left;">Age</td>
        </tr>
        <tr>
            <td style="text-align: left;">Income</td>
        </tr>
        <tr>
            <td style="text-align: left;">Education: Less than high school diploma</td>
            <td>{categoricals['EDUCATION'][ALL]['Less than 12th grade, no diploma']}%</td>
            <td>{categoricals['EDUCATION'][FAINT]['Less than 12th grade, no diploma']}%</td>
            <td>{categoricals['EDUCATION'][FLOURISHING]['Less than 12th grade, no diploma']}%</td>
            <td>{categoricals['EDUCATION'][FLOUNDERING]['Less than 12th grade, no diploma']}%</td>
        </tr>
        <tr>
            <td style="text-align: left;">Education: High school diploma</td>
            <td>{categoricals['EDUCATION'][ALL]['High school diploma (or equivalent)']}%</td>
            <td>{categoricals['EDUCATION'][FAINT]['High school diploma (or equivalent)']}%</td>
            <td>{categoricals['EDUCATION'][FLOURISHING]['High school diploma (or equivalent)']}%</td>
            <td>{categoricals['EDUCATION'][FLOUNDERING]['High school diploma (or equivalent)']}%</td>
        </tr>
        <tr>
            <td style="text-align: left;">Education: Some college</td>
            <td>{categoricals['EDUCATION'][ALL]['Some education after high school, no degree']}%</td>
            <td>{categoricals['EDUCATION'][FAINT]['Some education after high school, no degree']}%</td>
            <td>{categoricals['EDUCATION'][FLOURISHING]['Some education after high school, no degree']}%</td>
            <td>{categoricals['EDUCATION'][FLOUNDERING]['Some education after high school, no degree']}%</td>
        </tr>
        <tr>
            <td style="text-align: left;">Education: College degree</td>
            <td>{categoricals['EDUCATION'][ALL]['College degree (associate, bachelor’s, master’s, or doctorate)']}%</td>
            <td>{categoricals['EDUCATION'][FAINT]['College degree (associate, bachelor’s, master’s, or doctorate)']}%</td>
            <td>{categoricals['EDUCATION'][FLOURISHING]['College degree (associate, bachelor’s, master’s, or doctorate)']}%</td>
            <td>{categoricals['EDUCATION'][FLOUNDERING]['College degree (associate, bachelor’s, master’s, or doctorate)']}%</td>
        </tr>
        <tr>
            <td style="text-align: left;">Region: Midwest</td>
            <td>{categoricals['region4'][ALL]['Midwest']}%</td>
            <td>{categoricals['region4'][FAINT]['Midwest']}%</td>
            <td>{categoricals['region4'][FLOURISHING]['Midwest']}%</td>
            <td>{categoricals['region4'][FLOUNDERING]['Midwest']}%</td>
        </tr>
        <tr>
            <td style="text-align: left;">Region: Northeast</td>
            <td>{categoricals['region4'][ALL]['Northeast']}%</td>
            <td>{categoricals['region4'][FAINT]['Northeast']}%</td>
            <td>{categoricals['region4'][FLOURISHING]['Northeast']}%</td>
            <td>{categoricals['region4'][FLOUNDERING]['Northeast']}%</td>
        </tr>
        <tr>
            <td style="text-align: left;">Region: South</td>
            <td>{categoricals['region4'][ALL]['South']}%</td>
            <td>{categoricals['region4'][FAINT]['South']}%</td>
            <td>{categoricals['region4'][FLOURISHING]['South']}%</td>
            <td>{categoricals['region4'][FLOUNDERING]['South']}%</td>
        </tr>
        <tr>
            <td style="text-align: left;">Region: West</td>
            <td>{categoricals['region4'][ALL]['West']}%</td>
            <td>{categoricals['region4'][FAINT]['West']}%</td>
            <td>{categoricals['region4'][FLOURISHING]['West']}%</td>
            <td>{categoricals['region4'][FLOUNDERING]['West']}%</td>
        </tr>
    </table>
'''

filename = "output/descriptive_classes.html"
with open(filename, "w") as fh:
    print(f"Writing {filename}")
    fh.write(html)
