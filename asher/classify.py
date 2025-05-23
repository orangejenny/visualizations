import numpy as np
import pandas as pd

from functools import partial
from plotnine import *
from stepmix.stepmix import StepMix

from utils import (
    categorize_daily,
    convert_categorical_to_numeric,
    load_asher_data,
    CONSUMPTION_OPTIONS,
    SPECIES_KEYS,
)

freq = CONSUMPTION_OPTIONS.values()
red_options = [f for f in freq]
blue_options = [f1 + f2 for f1 in freq for f2 in freq]
white_options = [f1 + f2 + f3 for f1 in freq for f2 in freq for f3 in freq]

# High-level parameters for this run
levels = 4
weighted = True
measurement = "categorical" if levels else "continuous"

# Load data
data = load_asher_data()
item_counts = pd.DataFrame(data.count())
item_counts.loc[item_counts[0] > 2000,:].index.to_list()    # items in the 26K sample

# Convert consumption columns to numeric, and recall MEATDAILY is a summed numeric column
data = convert_categorical_to_numeric(data, SPECIES_KEYS, options=CONSUMPTION_OPTIONS)

# Add species groupings: by class (mammal / bird / fish) and perception (red meat / white meat / 'blue' meat)
data['MAMMALDAILY'] = data.apply(lambda df: sum([df[k] for k in ['BEEFDAILY', 'PORKDAILY']]), axis=1)
data['BIRDDAILY'] = data.apply(lambda df: sum([df[k] for k in ['CHICKENDAILY', 'TURKEYDAILY']]), axis=1)
data['REDDAILY'] = data.apply(lambda df: sum([df[k] for k in ['BEEFDAILY']]), axis=1)
data['WHITEDAILY'] = data.apply(lambda df: sum([df[k] for k in ['PORKDAILY', 'CHICKENDAILY', 'TURKEYDAILY']]), axis=1)
data['BLUEDAILY'] = data.apply(lambda df: sum([df[k] for k in ['FISHDAILY', 'SHELLFISHDAILY']]), axis=1)

# (ggplot(data.loc[data['MEATDAILY'] <= 5,:], aes(x = 'MEATDAILY')) + geom_histogram()).show()  # histogram of meat consumption
# (ggplot(data.loc[np.logical_and(data['MEATDAILY'] <= 3, data['PREVALENCES'] == 'Reducers'),:], aes(x = 'MEATDAILY')) + geom_histogram()).show()  # same, for semis only
# Among omnis, the percentages 0/1/2/3 are 13/26/48/12
# Among semis, the percentages 0/1/2/3 are 19/27/37/16
for key in ['MEATDAILY', 'MAMMALDAILY', 'BIRDDAILY', 'REDDAILY', 'WHITEDAILY', 'BLUEDAILY']:
    data[key] = data.apply(partial(categorize_daily, key=key, levels=levels), axis=1)

# Create diet-specific samples
#   Primarily interested in omnis and semis
meaters = data.loc[data['PREVALENCES'] != 'Vegetarians',:]
omnis = data.loc[data['PREVALENCES'] == 'Non-Reducing Omnivores',:]
semis = meaters.loc[data['PREVALENCES'] != 'Non-Reducing Omnivores',:]
reducers = data.loc[data['PREVALENCES'] == 'Reducers',:]
nochicks = data.loc[data['PREVALENCES'] == 'Chicken Avoiders',:]
print(data.loc[:,['PREVALENCES', 'MEATDAILY']].groupby('PREVALENCES', observed=True).count())    # unweighted, compare to Table 42 in Asher

# Primary modeling approach is categorical
#   That avoids continuous data making the data look more granular than it is
#   It's also much easier for people to categorize themselves than provide an accurate response to a continuous question
classes = ['MAMMALDAILY', 'BIRDDAILY', 'BLUEDAILY']
colors = ['REDDAILY', 'WHITEDAILY', 'BLUEDAILY']




def fit_model(data, num_classes, categories):
    weights = data['Wts'] if weighted else None
    data = data.loc[:,categories].copy()
    model = StepMix(n_components=num_classes, n_steps=3, measurement=measurement, verbose=0, random_state=23, max_iter=2000)
    model.fit(data, sample_weight=weights)
    data['pred'] = model.predict(data)
    if num_classes == 3:
        # Recode so that biggest class is first and will be used as reference category
        values = {0: 2, 1: 0, 2: 1}
        data['pred'] = data.apply(lambda df: values.get(df['pred'], df['pred']), axis=1)

    '''
    print("How many people are in each possible cell?")
    print(data.groupby(categories).count())
    print("How did each cell get split up?")
    print(data.reset_index().groupby(categories + ['pred']).count())
    '''

    return (model, data)


# Try from 2 to 12 classes. There are 64 cells.
# After doing this by both class and color, color-based models have lower AIC and BIC than class-based models
# For the semis:
#   2 classes is 90/10
#   3 classes has 1 small, 1 pretty small, and 1 big
#   4 classes has 2 big and 2 small ones
#   5 classes has 3 big and 2 small ones
'''
for i in range(1, 12):
    print(f"Fitting model with {i} classes...")
    fit_model(reducers, i, colors)
'''
indicators = ['REDDAILY', 'WHITEDAILY', 'BLUEDAILY']

# Elbow plot comparing different numbers of classes based on log likelihood
# Omnis also use three classes
'''
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.model_selection import GridSearchCV, ParameterGrid
model = StepMix(n_components=3, n_steps=1, measurement=measurement, random_state=23, max_iter=2000)
grid = {'n_components': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]}
gs = GridSearchCV(estimator=model, cv=5, param_grid=grid)
gs.fit(reducers.loc[:,indicators], sample_weight=reducers['Wts'])
results = pd.DataFrame(gs.cv_results_)
results["Validation Log Likelihood"] = results['mean_test_score']
results["Number of classes"] = results['param_n_components']
plot = sns.lineplot(data=results, x='Number of classes', y='Validation Log Likelihood')
plt.show()
'''



datasets = {
    #'omnis': omnis,
    'reducers': reducers,
    #'all non-veg': meaters,
    #'semis': semis,
}
greek_letters = [' "Superficial"', '  "Successful"', '"Struggling"', 'Delta', 'Epsilon', 'Zeta', 'Eta', 'Theta', 'Iota']
greek_letters = ['Alpha', 'Beta', 'Gamma', 'Delta', 'Epsilon', 'Zeta', 'Eta', 'Theta', 'Iota']
purples = {
    '3: most meals': '#4B0072',
    '2: most days': '#7A4CC6',
    '1: some days': '#9B6DCC',
    '0: seldom': '#D1A7E0',
}
blues = {
    '3: most meals': '#1E3A5F',
    '2: most days': '#2196F3',
    '1: some days': '#6BB9FF',
    '0: seldom': '#A7C7E7',
}
greens = {
    '3: most meals': '#2C6B2F',
    '2: most days': '#4CAF50',
    '1: some days': '#78D58B',
    '0: seldom': '#A8E6A3',
}
yellows = {
    '3: most meals': '#F57F17',
    '2: most days': '#FFB300',
    '1: some days': '#FFEB3B',
    '0: seldom': '#FFF9C4',
}
all_colors =    {
    'purples': purples,
    'blues': blues,
    'greens': greens,
    'yellows': yellows,
}
level_colors = purples
level_values = level_colors.keys()
for num_classes in [3, 4]:
    for dataset_label in datasets.keys():
        if not levels:
            # For continuous visualizations, cap values at 3, because otherwise the lower values aren't distinguishable
            datasets[dataset_label]['WHITEDAILY'] = datasets[dataset_label].apply(lambda df: min(df['WHITEDAILY'], 3), axis=1)
            datasets[dataset_label]['BLUEDAILY'] = datasets[dataset_label].apply(lambda df: min(df['BLUEDAILY'], 3), axis=1)

        weight_label = "_weighted" if weighted else ""
        filename = f"{num_classes} classes {dataset_label}{weight_label}.png"
        print(f"Generating model for {filename}")
        (model, predictions) = fit_model(datasets[dataset_label], num_classes, colors)
        cells = predictions.reset_index(names='count').groupby(indicators + ['pred'], as_index=False).count()

        class_counts = predictions.groupby('pred').count()['REDDAILY'].to_list()
        total = sum(class_counts)
        class_counts = [round(x * 100 / total) for x in class_counts]
        
        # Painfully format data because I am too tired to grasp pandas.melt
        viz_data = None
        for color in ['RED', 'WHITE', 'BLUE']:
            subset = cells.groupby(['pred', f'{color}DAILY'], as_index=False).sum()
            subset['color'] = "FISH" if color == "BLUE" else color
            subset['value'] = subset[f'{color}DAILY']
            subset = subset.loc[:,['pred', 'color', 'value', 'count']]
            if viz_data is None:
                viz_data = subset
            else:
                viz_data = pd.concat([viz_data, subset])

        # Plot with single bar of overall class proportions
        '''
        single_bar_data = pd.DataFrame.from_dict(class_counts)
        single_bar_data.rename({0: 'percent'}, axis=1, inplace=True)
        single_bar_data['x'] = 1
        single_bar_data.reset_index(inplace=True)
        single_level = '2: most days'   # which color shade to use for single bar graph
        single_bar_colors = {
            '  "Superficial"': blues[single_level],
            '"Successful"': greens[single_level],
            '   "Struggling"': yellows[single_level],
        }
        single_bar_labels = list(single_bar_colors.keys())
        single_bar_data['label'] = single_bar_data.apply(lambda df: single_bar_labels[df['index']], axis=1)
        plot = (
            ggplot(single_bar_data, aes(x = 'x', y = 'percent', fill = 'label'))
            + geom_bar(position = "fill", stat = "identity")
            + scale_fill_manual(values=single_bar_colors, limits=single_bar_labels)
            + labs(x = "", y = "")
            + theme_classic(base_size=24)
            + theme(legend_position="none")
            + coord_flip()
        )
        plot.show()
        '''
        
        value_lookup = ['seldom', 'some days', 'most days', 'most meals']
        if levels == 5:
            value_lookup = ['never'] + value_lookup
        if levels:
            viz_data['value'] = viz_data.apply(lambda df: str(df['value']) + ': ' + value_lookup[df['value']], axis=1)

        # Plot of class details
        viz_data['pred'] = viz_data.apply(lambda df: f"{greek_letters[df['pred']]}{num_classes} ({class_counts[df['pred']]}%)", axis=1)
        #viz_data['pred'] = viz_data.apply(lambda df: f"{greek_letters[df['pred']]}", axis=1)
        for colors_label, level_colors in all_colors.items():
            plot = (
                ggplot(viz_data, aes(x = 'color', y = 'count', fill = 'factor(value)' if levels else 'value'))
                + geom_bar(position = "fill", stat = "identity") + facet_wrap('pred', nrow=1)
                #+ scale_fill_grey(start=0.8, end=0.2)
                + scale_fill_manual(values=level_colors, limits=list(level_colors.keys()))
                + labs(x = "", y = "")
                + theme_classic(base_size=24)
                + theme(legend_position="none")
                #+ theme(legend_position="left" if num_classes == 2 else "right")
            )
            plot.show()
            #plot.save(filename=f"stacked_class_viz_{levels}_levels/{colors_label}_{filename}")
exit(0)


# Write output for regress.py to pick up
label = 'omnis'
num_classes = 3
(model, predictions) = fit_model(datasets[label], num_classes, colors)
output = datasets[label].join(predictions, how='inner', rsuffix='_model')
output.drop([f'{c}_model' for c in colors], axis=1, inplace=True)
weight_suffix = "_weighted" if weighted else ""
output.to_csv(f"{label}_{num_classes}_classes_with_pred{weight_suffix}.csv")
