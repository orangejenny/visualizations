'''
This creates an HTML table of descriptive statistics of the flexitarian sample and the classes within it.
'''
import pandas as pd

from utils import (
    counts_dict,
)

FAINT = 0
FLOURISHING = 1
FLOUNDERING = 2


data = pd.read_csv("reducers_3_classes_with_pred_weighted.csv")
class_counts = counts_dict(data, 'pred')
n_all = sum(class_counts.values())

html = f'''
    <table style="text-align:center">
        <!-- Class labels -->
        <tr>
            <td></td>
            <td colspan="2">All Flexitarians<br>(n={n_all})</td>
            <td colspan="2">Faint<br>(n={class_counts[FAINT]})</td>
            <td colspan="2">Flourishing<br>(n={class_counts[FLOURISHING]})</td>
            <td colspan="2">Floundering<br>(n={class_counts[FLOUNDERING]})</td>
        </tr>
        <!-- Count + Percentage labels -->
        <tr>
            <td></td>
            <td>Count</td>
            <td>% or Mean (Std Dev)</td>
            <td>Count</td>
            <td>% or Mean (Std Dev)</td>
            <td>Count</td>
            <td>% or Mean (Std Dev)</td>
            <td>Count</td>
            <td>% or Mean (Std Dev)</td>
        </tr>
        <!-- Demographic rows -->
        <tr>
            <td style="text-align: left;">Gender: Men</td>
        </tr>
        <tr>
            <td style="text-align: left;">Race: White</td>
        </tr>
        <tr>
            <td style="text-align: left;">Age</td>
        </tr>
        <tr>
            <td style="text-align: left;">Income</td>
        </tr>
        <tr>
            <td style="text-align: left;">Education: Less than high school diploma</td>
        </tr>
        <tr>
            <td style="text-align: left;">Education: High school diploma</td>
        </tr>
        <tr>
            <td style="text-align: left;">Education: Some college</td>
        </tr>
        <tr>
            <td style="text-align: left;">Education: College degree</td>
        </tr>
        <tr>
            <td style="text-align: left;">Region: Midwest</td>
        </tr>
        <tr>
            <td style="text-align: left;">Region: Northeast</td>
        </tr>
        <tr>
            <td style="text-align: left;">Region: South</td>
        </tr>
        <tr>
            <td style="text-align: left;">Region: West</td>
        </tr>
    </table>
'''

filename = "output/descriptive_classes.html"
with open(filename, "w") as fh:
    print(f"Writing {filename}")
    fh.write(html)
