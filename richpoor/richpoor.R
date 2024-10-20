# Exploratory: what it means to be rich/poor
setwd("~/Documents/visualizations")
data <- read_csv("~/Downloads/VOTER Panel Data Files/voter_panel.csv")

math_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

ecol <- function (col_name) {
  return(parse(text=as.name(col_name)))
}

stats <- function(df, metric) {
  return(
    df %>% summarise(
      min = min(eval(ecol(metric))),
      mean = mean(eval(ecol(metric))),
      median = median(eval(ecol(metric))),
      mode = math_mode(eval(ecol(metric))),
      sd = sd(eval(ecol(metric))),
      max = max(eval(ecol(metric))),
      var = var(eval(ecol(metric))),
      count = n(),
    )
  )
}

bank <- data %>%
  filter(!is.na(bankrich_2019Jan)) %>% # leaves 6779 rows, everyone answered either both or neither
  mutate(
    bankdiff = bankrich_2019Jan - bankpoor_2019Jan,
    bankratio = bankrich_2019Jan / bankpoor_2019Jan
  )
  # TODO: add demographics: age buckets, income buckets

ggplot(bank, aes(x = bankrich_2019Jan)) +
  geom_histogram(fill = "steelblue") +
  scale_x_log10(breaks=c(0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000),
                labels=c(0, 10, 100, "1K", "10K", "100K", "1M", "10M", "100M", "1B"))

# min -8, maybe filter out negatives
# mean 54M, mean isn't especially useful here, maybe drop outliers
stats(bank, "bankrich_2019Jan")

ggplot(bank, aes(x = bankpoor_2019Jan)) +
  geom_histogram(fill = "steelblue") +
  scale_x_log10(breaks=c(0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000),
                labels=c(0, 10, 100, "1K", "10K", "100K", "1M", "10M", "100M", "1B"))

# min -100K, maybe means debt?
# mean 254K, again not useful
# median $100
# mode is zero
stats(bank, "bankpoor_2019Jan")
poor_mean = mean(bank$bankpoor_2019Jan)
poor_sd <- sd(bank$bankpoor_2019Jan)
poor_no_outliers <- bank %>% 
  filter(bankpoor_2019Jan <= 100000) %>% 
  filter(bankpoor_2019Jan >= 0)
stats(poor_no_outliers, "bankpoor_2019Jan") # mean is now $2340, SD $8741

# Histogram of difference between rich and poor, not that interesting because modes are so dominant
ggplot(bank, aes(x = bankdiff)) +
  geom_histogram(fill = "steelblue") +
  scale_x_log10(breaks=c(0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000),
                labels=c(0, 10, 100, "1K", "10K", "100K", "1M", "10M", "100M", "1B"))

ggplot(bank %>% filter(bankpoor_2019Jan > 0), aes(x = bankratio)) +
  geom_histogram(fill = "steelblue") +
  scale_x_log10(breaks=c(0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000),
                labels=c(0, 10, 100, "1K", "10K", "100K", "1M", "10M", "100M", "1B"))
stats(bank %>% filter(bankpoor_2019Jan > 0), "bankratio") # mode 1000x, median 2000x

stats(bank %>% group_by(race_2019Nov), "bankrich_2019Jan")
stats(bank %>% group_by(race_2019Nov), "bankpoor_2019Jan")
stats(bank %>% group_by(educ_2019Nov), "bankpoor_2019Jan")
stats(bank %>% group_by(inputstate_2020Nov), "bankrich_2019Jan")
stats(bank %>% group_by(inputstate_2020Nov), "bankpoor_2019Jan")

# Who does this data represent?...it's weighted
# Stats!
# Demographics to look at...facet! Or group by and look at stats
#   income: am *I* rich? probably can't tell exactly: add quintiles
#   age: birthyr_2011
#   region: have state (inputstate_2020Nov) and congressional district id, could look at urbanity
#   race
#   ideology, could also look specifically at socialist (eat the rich) and libertarian
#   employment
#   number of children
#   education
# Does this map to meaning?
#   People like round numbers
#   What if more context were given, like poverty line or income deciles? How does this compare to those?
