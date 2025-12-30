import numpy as np
import pandas as pd

from plotnine import (
    aes,
    annotate,
    coord_flip,
    geom_col,
    geom_hline,
    geom_line,
    geom_point,
    ggplot,
    labs,
    scale_fill_manual,
    scale_y_continuous,
    theme,
    theme_minimal,
)


def year_of_data(df, year):
    return _data_contains(df, "Survey Year", year)

def female_data(df):
    return _data_contains(df, "Dose", ", Females")

def male_data(df):
    return _data_contains(df, "Dose", ", Males")

def all_gender_data(df):
    return _data_contains(df, "Dose", ", Males and Females")

def _data_contains(df, field, value):
    return df.loc[df[field].str.endswith(str(value)), :].copy()

# All HPV data
data = pd.read_csv("Vaccination_Coverage_among_Adolescents.csv")
assert set(data['Vaccine/Sample']) == {'HPV'}
data.drop(['Vaccine/Sample'], axis=1, inplace=True)
data.drop(['Dimension Type'], axis=1, inplace=True)

# Filter to state-level UTD stats for 13-17-year-olds
data = data.loc[data['Geography Type'] == 'States/Local Areas', :]
data.drop(['Geography Type'], axis=1, inplace=True)
data = data.loc[data['Dimension'] == '13-17 Years', :]
data.drop(['Dimension'], axis=1, inplace=True)
data = data.loc[data['Dose'].str.startswith("Up-to-Date"), :]
data = data.loc[~data['Geography'].str.contains('-'), :]        # filter out sub-geographies of states

# Construct data by state
female = female_data(data)
male = male_data(data)
key = ['Geography', 'Survey Year']
gaps = pd.merge(male, female, how='inner', left_on=key, right_on=key, suffixes=("_male", "_female"))
gaps['Gap'] = gaps['Estimate (%)_female'] - gaps['Estimate (%)_male']

both = all_gender_data(data)
gaps = pd.merge(gaps, both, how='inner', left_on=key, right_on=key, suffixes=("", ""))
gaps.drop([col for col in gaps.columns if col.endswith("_male") or col.endswith("_female")], axis=1, inplace=True)

years = pd.DataFrame(sorted(list(set(gaps['Geography']))))
years.rename(lambda x: 'Geography', axis=1, inplace=1)

for year in sorted(set(gaps['Survey Year'])):
    year_gaps = gaps.loc[gaps['Survey Year'] == str(year), ['Gap', 'Geography']]
    years = pd.merge(years, year_gaps, how='inner', left_on='Geography', right_on='Geography', suffixes=("", f"_{year}"))

years['Gap_2016'] = years['Gap']
years.drop('Gap', axis=1, inplace=True)

# Aggregate to see minimum and maximum gaps by state
min_gaps = gaps.loc[:,['Geography','Gap']].groupby(['Geography']).min()
max_gaps = gaps.loc[:,['Geography','Gap']].groupby(['Geography']).max()
agg_gaps = pd.merge(min_gaps, max_gaps, how='inner', left_on='Geography', right_on='Geography', suffixes=("_min", "_max"))
agg_gaps['Gap_diff'] = agg_gaps['Gap_max'] - agg_gaps['Gap_min']
agg_gaps['Gap_mult'] = agg_gaps['Gap_min'] * agg_gaps['Gap_max']
agg_gaps['Gap_signs'] = agg_gaps['Gap_mult'] / np.abs(agg_gaps['Gap_mult'])  # -1 if signs are ever different, 1 if they're always the same

# Plot
years['Gap_2023_pos'] = np.less(0, years['Gap_2023'])
estimates_2023 = both.loc[both['Survey Year'] == '2023',['Geography', 'Estimate (%)']]
years = pd.merge(years, estimates_2023, how='inner', left_on='Geography', right_on='Geography')
years['Estimate_2023'] = years['Estimate (%)']
years.drop('Estimate (%)', axis=1, inplace=True)
basic_plot = (
    ggplot(years, aes(x = "reorder(Geography, Gap_2023)", y = "Gap_2023", fill = "Gap_2023_pos"))
        + geom_col()
        + geom_hline(yintercept=5, color="#333333", size=0.5, linetype="dotted")  # National gap: 5%, since boys are 59% and girls are 64%, all in 2023
        + theme_minimal()
        + scale_fill_manual(values=({False: "#669966", True: "#ff9966"}))
        + theme(legend_position="none")
        + coord_flip()
        + annotate("text", x=51, y=-8, label="More girls vaccinated", size=10)
        + annotate("text", x=5, y=-17, label="More boys vaccinated", size=10)
)
plot1 = (
    basic_plot
        + scale_y_continuous(limits = [-25, 25], labels = lambda labels: [f"{abs(round(x))}%" for x in labels])
        + labs(x = "", y = "2023 HPV vaccination gaps between boys and girls 13-17 years old", title = "")
        + annotate("text", x=25, y=11, label="National gap (5%)", size=10)
        + theme(figure_size=(7, 8))
)
plot1.show()

plot2 = (
    basic_plot
        + geom_point(aes(y = "Estimate_2023"))
        + scale_y_continuous(limits = [-25, 100], labels = lambda labels: [f"{abs(round(x))}%" for x in labels])
        + theme(figure_size=(7 * 5 / 2.0, 8))
        + geom_hline(yintercept=61.4, color="#333333", size=0.5, linetype="dotted")  # National rate: 61.4%
        + labs(x = "", y = "2023 HPV vaccination rates and gaps between boys and girls 13-17 years old", title = "")
        + annotate("text", x=49, y=69, label="National rate (61%)", size=10)
        + annotate("text", x=25, y=12, label="National gap (5%)", size=10)
)
plot2.show()

gaps["Survey Year"] = pd.to_numeric(gaps["Survey Year"], errors = "coerce")
gaps["Michigan"] = np.equal(gaps["Geography"], "Michigan") * 1 + 0.5
noisy_plot = (
    ggplot(gaps, aes(x = "Survey Year", y = "Gap", color = "Geography"))
        + geom_line()   # aes(size = "Michigan"))
        + geom_hline(yintercept=0, color="#000000", size=0.5, linetype="solid")
        + scale_y_continuous(limits = [-25, 25], labels = lambda labels: [f"{round(x)}%" for x in labels])
        + theme_minimal()
        + theme(legend_position="none")
        + labs(x = "Year", y = "HPV vaccination rate gender gap", title = "")
)
noisy_plot.show()
pass
