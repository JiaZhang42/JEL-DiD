# load packages
library(tidyverse)
library(kableExtra)
library(modelsummary)
library(gt)
library(did)
library(ggthemes)
library(fixest)
library(latex2exp)
library(HonestDiD)

# load cleaned data
mydata <- read_csv(here::here("data", "county_mortality_data.csv"))
mydata %>% distinct(county) %>% nrow()
3064 * 11 # 33704
mydata %>% nrow() # 31843, unbalanced panel
mydata %>% count(county) %>% filter(n != 11)


mydata <- read_csv(here::here("data", "county_mortality_data.csv")) %>% 
  # make state the abbreviation
  mutate(state = str_sub(county, nchar(county) - 1, nchar(county))) %>%
  # drop District of Columbia from the data
  filter(state != "DC") %>% 
  # make a second adoption variable for the table
  mutate(adopt = case_when(
    # missing is non-expansion
    is.na(yaca) ~ "Non-Expansion",
    # fix a couple pre-2014 adoptions from Miller, Johnson and Wherry
    state %in% c("DE", "MA", "NY", "VT") ~ "Pre-2014", 
    TRUE ~ as.character(yaca)
  ))

# table 1 ---------

# get adoption year by state
adopts <- mydata %>% 
  distinct(state, adopt) %>% 
  arrange(state)

# first get the share of states, share of counties, and share of adults in 2013 by adoption category
# first the states and share of states
states <- adopts %>% 
  group_by(adopt) %>% 
  summarize(states = paste0(state, collapse = ", "),
            state_share = n() / nrow(adopts))

# next get the county share and the population share
counties_pop <- mydata %>% 
  # just for year 2013
  filter(year == 2013) %>% 
  # get county and population totals
  mutate(total_counties = n(),
         total_pop = sum(population_20_64, na.rm = TRUE)) %>% 
  group_by(adopt) %>% 
  # make into shares
  summarize(county_share = n() / mean(total_counties),
            pop_share = sum(population_20_64, na.rm = TRUE) / mean(total_pop))

# make a table and export
states %>% 
  left_join(counties_pop, by = "adopt") %>% 
  slice(9, 1:8) %>% 
  # format the numbers to two digits
  mutate(across(state_share:pop_share, ~ scales::number(., accuracy = 0.01))) %>%
  # format the table
  kable(
    col.names = c("Expansion \n Year", "States", "Share of States", "Share of Counties", 
                  "Share of Adults (2013)"),
    booktabs = T, caption = "Medicaid Expansion Under the Affordable Care Act",
    label = "adoptions",
    align = c("c"),
    escape = FALSE,
    linesep = ""
  ) %>% 
  kable_styling(latex_options = c("scale_down", "HOLD_position")) %>%
  column_spec(2, width = "20em")

# 2x2 --------

# set seed for reproducibility
set.seed(20240924)

# set filepath locations for tables and figures
file.loc.tab <- here::here("tables/")
file.loc.fig <- here::here("figures/")

# load cleaned data
mydata <- read_csv(here::here("data", "county_mortality_data.csv")) %>% 
  # make state the abbreviation
  mutate(state = str_sub(county, nchar(county) - 1, nchar(county))) %>%
  # drop DC and pre-2014 adoption states
  filter(!(state %in% c("DC", "DE", "MA", "NY", "VT"))) %>% 
  # drop states that adopt between 2014 and 2019
  filter(yaca == 2014 | is.na(yaca) | yaca > 2019)

# set the covariates that we're going to use.
covs <- c("perc_female","perc_white", "perc_hispanic", "unemp_rate", "poverty_rate", "median_income")

## Clean data and add in covariates
mydata <- mydata %>% 
  # make variables
  mutate(perc_white = population_20_64_white / population_20_64 * 100,
         perc_hispanic = population_20_64_hispanic / population_20_64 * 100,
         perc_female = population_20_64_female/ population_20_64 * 100,
         unemp_rate = unemp_rate * 100,
         median_income = median_income / 1000) %>% 
  # keep just subset of variables that we will use later
  select(state, county, county_code, year, population_20_64, yaca,
         starts_with("perc_"), crude_rate_20_64, all_of(covs))

# keep only counties with full observations for outcome and covariates in 2013 and 2014
mydata <- mydata %>%
  # allow the aca expansion variable to be missing
  drop_na(!yaca) %>%
  group_by(county_code) %>% 
  # need full covariates for 2013 and 2014
  filter(length(which(year == 2013 | year == 2014)) == 2) %>% 
  ungroup()

# finally, keep only counties with full mortality data for 2009 to 2019
mydata <- mydata %>% 
  group_by(county_code) %>% 
  drop_na(crude_rate_20_64) %>% 
  filter(n() == 11)

# make a smaller dataset with only years 2013 and 2014
short_data <- mydata %>% 
  # make a binary variable that identifies ACA expansion and post-years
  mutate(Treat = if_else(yaca == 2014 & !is.na(yaca), 1, 0),
         Post = if_else(year == 2014, 1, 0)) %>% 
  # filter years for just 2013 and 2014
  filter(year %in% c(2013, 2014)) %>% 
  # make a variable with population weight in 2013
  group_by(county_code) %>% 
  mutate(set_wt = population_20_64[which(year == 2013)]) %>% 
  ungroup()

## get group means for Table 2 ----
# unweighted expansion pre-reform
T_pre <- short_data %>% 
  filter(Treat == 1 & year == 2013) %>% 
  summarize(mean = mean(crude_rate_20_64)) %>% 
  pull(mean)

# unweighted non-expansion pre-form 
C_pre <- short_data %>% 
  filter(Treat == 0 & year == 2013) %>% 
  summarize(mean = mean(crude_rate_20_64)) %>% 
  pull(mean)

# unweighted expansion post-reform
T_Post <- short_data %>% 
  filter(Treat == 1 & year == 2014) %>% 
  summarize(mean = mean(crude_rate_20_64)) %>% 
  pull(mean)

# unweighted non-expansion post-reform
C_Post <- short_data %>% 
  filter(Treat == 0 & year == 2014) %>% 
  summarize(mean = mean(crude_rate_20_64)) %>% 
  pull(mean)

# get the same group means but this time weighted
T_pre_weight <- short_data %>% 
  filter(Treat == 1 & year == 2013) %>% 
  summarize(mean = weighted.mean(crude_rate_20_64, w = set_wt)) %>% 
  pull(mean)

C_pre_weight <- short_data %>% 
  filter(Treat == 0 & year == 2013) %>% 
  summarize(mean = weighted.mean(crude_rate_20_64, w = set_wt)) %>% 
  pull(mean)

T_Post_weight <- short_data %>% 
  filter(Treat == 1 & year == 2014) %>% 
  summarize(mean = weighted.mean(crude_rate_20_64, w = set_wt)) %>% 
  pull(mean)

C_Post_weight <- short_data %>% 
  filter(Treat == 0 & year == 2014) %>% 
  summarize(mean = weighted.mean(crude_rate_20_64, w = set_wt)) %>% 
  pull(mean)

# function to do a reasonable rounding
g_round <- function(x, k) {format(round(x, k), nsmall = k)} # keep 0s at the tail

# make table and export
tribble(
  ~"", ~"Expansion", ~ "No Expansion", ~"Gap/DiD", ~"Expansion", ~"No Expansion", ~"Gap/DiD",
  "2013", as.character(g_round(T_pre, 1)), as.character(g_round(C_pre, 1)), as.character(g_round(T_pre - C_pre, 1)), 
  as.character(g_round(T_pre_weight, 1)), as.character(g_round(C_pre_weight, 1)), as.character(g_round(T_pre_weight - C_pre_weight, 1)),
  "2014", as.character(g_round(T_Post, 1)), as.character(g_round(C_Post, 1)), as.character(g_round(T_Post - C_Post, 1)), 
  as.character(g_round(T_Post_weight, 1)), as.character(g_round(C_Post_weight, 1)), as.character(g_round(T_Post_weight - C_Post_weight, 1)),
  "Trend/DiD", as.character(g_round(T_Post - T_pre, 1)), as.character(g_round(C_Post-C_pre, 1)), 
  as.character(g_round((T_Post - T_pre) - (C_Post-C_pre), 1)),
  as.character(g_round(T_Post_weight - T_pre_weight, 1)), as.character(g_round(C_Post_weight-C_pre_weight, 1)), 
  as.character(g_round((T_Post_weight - T_pre_weight) - (C_Post_weight-C_pre_weight), 1))
) %>% 
  # format table
  kable(align = 'c',
        booktabs = T, 
        escape = F,
        caption = "Simple 2 X 2 DiD") %>% 
  kable_styling() %>% 
  row_spec(3, color = "BrickRed") %>% 
  column_spec(c(1:3, 5:6), color = "black") %>% 
  row_spec(3, italic = TRUE) %>% 
  column_spec(c(4, 7), italic = TRUE) %>% 
  add_header_above(c(" " = 1, "Unweighted Averages" = 3, "Weighted Averages" = 3))

## show that you can get the same estimates with regression ----
# make a different short data with long differences in mortality by county and treatment indicators
short_data2 <- short_data %>% 
  group_by(county_code) %>% 
  summarize(state = state[1],
            set_wt = mean(set_wt),
            # long difference between 2014 and 2013 rates
            diff = crude_rate_20_64[which(year == 2014)] - crude_rate_20_64[which(year == 2013)],
            Treat = mean(Treat),
            Post = 1)

# estimate three models without weights. These are Treat*Post with no fixed effects,
# fixed effects + Treat:Post, and then the long difference model with no fixed effects.
mod1 <- feols(crude_rate_20_64 ~ Treat*Post, data = short_data, cluster = ~county_code)
mod11 <- feols(crude_rate_20_64 ~ Treat:Post | Treat + year, data = short_data, cluster = ~county_code) # equivalent to mod1, just formatted as FE
mod2 <- feols(crude_rate_20_64 ~ Treat:Post | county_code + year, data = short_data, cluster = ~county_code) # change from cohort FE to individual FE, doesn't change results because Treat x Post only varies at the cohort level, and its ts-demeaned value is orthogonal to the residual county FE (see Bruce Hansen on DiD)
mod3 <- feols(diff ~ Treat:Post, data = short_data2, cluster = ~county_code) #note here the regressor is different from the previous ones, it is acturally D_i (Treat)

# estimate the same three three models with weights
mod4 <- feols(crude_rate_20_64 ~ Treat*Post, data = short_data, weights = ~set_wt, cluster = ~county_code)
mod5 <- feols(crude_rate_20_64 ~ Treat:Post | county_code + year, data = short_data, 
              weights = ~set_wt, cluster = ~county_code)
mod6 <- feols(diff ~ Treat:Post, data = short_data2, weights = ~set_wt, cluster = ~county_code)


# this dictionary maps names to labels for the table
dict <- c("(Intercept)" = "Constant",
          "Treat" = "Medicaid Expansion",
          "Post" = "Post",
          "Treat:Post" = "Medicaid Expansion X Post")

# rows to add to table
rows = tribble(
  ~x1, ~x2, ~x3, ~x4, ~x5, ~x6, ~x7,
  "County fixed effects", "No", "Yes", "No", "No", "Yes", "No",
  "Year fixed effects", "No", "Yes", "No", "No", "Yes", "No"
)

# export table 3
modelsummary(list("(1)" = mod1, "(2)" = mod2, "(3)" = mod3, 
                  "(4)" = mod4, "(5)" = mod5, "(6)" = mod6),
             coef_map = dict, escape = FALSE, gof_map = NA,
             fmt = 1, stars = FALSE,
             estimate = "{estimate}", add_rows = rows,
             title = "Regression 2 X 2 DiD", 
             output = 'kableExtra') %>%
  add_header_above(c(" " = 1, "Crude Morality Rate" = 2, "\\(\\Delta\\)" = 1, 
                     "Crude Mortality Rate" = 2, "\\(\\Delta\\)" = 1), escape = FALSE)

## Now, make the covariate balance table (Table 4).-------
# unweighted - pre
unweighted <- short_data %>% 
  filter(year == 2013) %>% 
  select(Treat, all_of(covs)) %>% 
  group_by(Treat) %>% 
  # get mean and standard deviation
  summarize_all(list(mean, var)) %>%
  # pivot the data longer
  pivot_longer(cols = !Treat, 
               names_to = "variable", 
               values_to = "value") %>% 
  # now make separate columns for treated and untreated
  pivot_wider(names_from = "Treat", 
              values_from = "value",
              names_prefix = "group") %>% 
  # separate mean and standard deviations
  extract(variable, into = c("variable", "fx"), "(.*)_(.*)") %>% 
  pivot_wider(id_cols = variable,
              names_from = fx,
              values_from = c(group0, group1)) %>% 
  # make normalized difference
  mutate(norm_diff = (group1_fn1 - group0_fn1)/sqrt((group1_fn2 + group0_fn2)/2)) %>% 
  select(variable, group0_fn1, group1_fn1, norm_diff)

# make a weighted variance function
wtd.var <- function (x, weights = NULL, normwt = FALSE, na.rm = TRUE, 
                     method = c("unbiased", "ML")) 
{
  method <- match.arg(method)
  if (!length(weights)) {
    if (na.rm) 
      x <- x[!is.na(x)]
    return(var(x))
  }
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  if (normwt) 
    weights <- weights * length(x)/sum(weights)
  if (normwt || method == "ML") 
    return(as.numeric(stats::cov.wt(cbind(x), weights, method = method)$cov))
  sw <- sum(weights)
  if (sw <= 1) 
    warning("only one effective observation; variance estimate undefined")
  xbar <- sum(weights * x)/sw
  sum(weights * ((x - xbar)^2))/(sw - 1)
}

# weighted  - pre
weighted <- short_data %>% 
  filter(year == 2013) %>% 
  select(Treat, all_of(covs), set_wt) %>% 
  group_by(Treat) %>% 
  # get mean and standard deviation
  summarize(across(all_of(covs), 
                   list(
                     ~weighted.mean(x = ., w = set_wt),
                     ~wtd.var(x = ., weights = set_wt, normwt = TRUE)))) %>%
  # pivot the data longer
  pivot_longer(cols = !Treat, 
               names_to = "variable", 
               values_to = "value") %>% 
  # now make separate columns for treated and untreated
  pivot_wider(names_from = "Treat", 
              values_from = "value",
              names_prefix = "group") %>% 
  # separate mean and standard deviations
  extract(variable, into = c("variable", "fx"), "(.*)_(.*)") %>% 
  pivot_wider(id_cols = variable,
              names_from = fx,
              values_from = c(group0, group1)) %>% 
  # make normalized difference
  mutate(norm_diff = (group1_1 - group0_1)/sqrt((group1_2 + group0_2)/2)) %>% 
  select(variable, group0_1, group1_1, norm_diff)

# make the top panel (weighted and unweighted Pre)
top_panel <- bind_cols(unweighted, weighted %>% select(-variable))

# make the bottom panel, which is the same thing but with the difference in X between 2014 and 2013.
# unweighted
unweighted <- short_data %>% 
  select(county_code, year, Treat, all_of(covs)) %>% 
  arrange(county_code, year) %>% 
  group_by(county_code, Treat) %>% 
  summarize(
    across(all_of(covs), function(x) x[2] - x[1])
  ) %>% 
  ungroup() %>% 
  select(-county_code) %>% 
  group_by(Treat) %>% 
  # get mean and standard deviation
  summarize_all(list(mean, var)) %>%
  # pivot the data longer
  pivot_longer(cols = !Treat, 
               names_to = "variable", 
               values_to = "value") %>% 
  # now make separate columns for treated and untreated
  pivot_wider(names_from = "Treat", 
              values_from = "value",
              names_prefix = "group") %>% 
  # separate mean and standard deviations
  extract(variable, into = c("variable", "fx"), "(.*)_(.*)") %>% 
  pivot_wider(id_cols = variable,
              names_from = fx,
              values_from = c(group0, group1)) %>% 
  # make normalized difference
  mutate(norm_diff = (group1_fn1 - group0_fn1)/sqrt((group1_fn2 + group0_fn2)/2)) %>% 
  select(variable, group0_fn1, group1_fn1, norm_diff)

# weighted
weighted <- short_data %>% 
  select(county_code, year, Treat, all_of(covs)) %>% 
  arrange(county_code, year) %>% 
  group_by(county_code, Treat) %>% 
  summarize(
    across(all_of(covs), function(x) x[2] - x[1])
  ) %>% 
  ungroup() %>% 
  left_join(short_data %>% filter(year == 2013) %>% select(county_code, set_wt),
            join_by(county_code)) %>% 
  select(-county_code) %>% 
  group_by(Treat) %>%
  # get mean and standard deviation
  summarize(across(all_of(covs), 
                   list(
                     ~weighted.mean(x = ., w = set_wt),
                     ~wtd.var(x = ., weights = set_wt, normwt = TRUE)))) %>%
  # pivot the data longer
  pivot_longer(cols = !Treat, 
               names_to = "variable", 
               values_to = "value") %>% 
  # now make separate columns for treated and untreated
  pivot_wider(names_from = "Treat", 
              values_from = "value",
              names_prefix = "group") %>% 
  # separate mean and standard deviations
  extract(variable, into = c("variable", "fx"), "(.*)_(.*)") %>% 
  pivot_wider(id_cols = variable,
              names_from = fx,
              values_from = c(group0, group1)) %>% 
  # make normalized difference
  mutate(norm_diff = (group1_1 - group0_1)/sqrt((group1_2 + group0_2)/2)) %>% 
  select(variable, group0_1, group1_1, norm_diff)

# make the bottom panel
bottom_panel <- bind_cols(unweighted, weighted %>% select(-variable))

# bind the two panels
table <- bind_rows(top_panel, bottom_panel) %>% 
  # reformat all columns to two digits
  mutate(across(-variable, \(x) scales::comma(x, accuracy = 0.01)))

# format Table 4 and export
table %>% 
  # change column names
  mutate(variable = 
           case_match(variable,
                      "perc_female" ~ "% Female",
                      "perc_white" ~ "% White",
                      "perc_hispanic" ~ "% Hispanic",
                      "unemp_rate" ~ "Unemployment Rate",
                      "poverty_rate" ~ "Poverty Rate",
                      "median_income" ~ "Median Income")) %>% 
  # format latex table
  kable(col.names = c("Variable", "Non-Adopt", "Adopt", "Norm. Diff.", "Non-Adopt", "Adopt", "Norm. Diff."),
        align = 'lcccccc',
        escape = T,
        booktabs = T,
        label = "cov_balance",
        caption = "Covariate Balance Statistics",
        linesep = "") %>% 
  kable_styling(latex_options = c("scale_down", "hold_position")) %>% 
  pack_rows("2013 Covariate Levels", 1, 6) %>% 
  pack_rows("2014 - 2013 Covariate Differences", 7, 12) %>% 
  add_header_above(c(" " = 1, "Unweighted" = 3, "Weighted" = 3))

