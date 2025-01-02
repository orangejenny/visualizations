from plotnine import (
    aes,
    element_text,
    geom_histogram,
    ggplot,
    labs,
    theme,
)


def histogram(data, metric):
    plot = (
        ggplot(data, aes(x = metric))
        + geom_histogram()   #binwidth = 5)
        + theme(axis_text_x=element_text(rotation = 90))
        + labs(x = metric, y = "")
    )
    plot.show()
