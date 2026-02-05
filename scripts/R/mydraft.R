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

# table 1: the expansion corhorts ---------

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

# 2 x 2 --------

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

## table 2: grouped means----
# get group means for Table 2
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

## regression DiD ----
# show that you can get the same estimates with regression
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


## covariate balance table ----
# Now, make the covariate balance table (Table 4).
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

## Regression 2 x 2 DiD with Covariates ----

# make a table that includes the model 1) without covariates, 2) long regression with 2013 covariates values, 3) long regression with *difference* in covariate values.
# all use the long difference model with delta Y as the outcome
# corresponds to reg equations on page 20 of the paper
reg_data_2013 <- short_data %>% 
  # make long diff in y
  group_by(county_code) %>% 
  summarize(long_y = crude_rate_20_64[which(year == 2014)] - crude_rate_20_64[which(year == 2013)]) %>% 
  # merge in 2013 covariates
  left_join(short_data %>% filter(year == 2013) %>% select(county_code, state, Treat, set_wt, all_of(covs)), 
            by = "county_code")

# this dataset has the change in X between 2014 and 2013 as controls
reg_data_change <- short_data %>% 
  # make long diff in y
  group_by(county_code) %>% 
  summarize(long_y = crude_rate_20_64[which(year == 2014)] - crude_rate_20_64[which(year == 2013)]) %>% 
  # merge in change in covariate values
  left_join(short_data %>% 
              group_by(county_code) %>% 
              mutate(set_wt = set_wt[which(year == 2013)]) %>% 
              group_by(county_code, state, Treat, set_wt) %>% 
              summarize(
                across(all_of(covs), function(x) x[which(year == 2014)] - x[which(year == 2013)])
              ), by = "county_code")

# run the six models
# first unweighted - long diff no covs, 2013 covs, then change in covs
mod1 <- feols(long_y ~ Treat, data = reg_data_2013, cluster = ~county_code)
mod2 <- feols(long_y ~ Treat + .[covs], data = reg_data_2013, cluster = ~county_code)
mod3 <- feols(long_y ~ Treat + .[covs], data = reg_data_change, cluster = ~county_code)

# same thing but weighted
mod4 <- feols(long_y ~ Treat, data = reg_data_2013, weights = ~set_wt, cluster = ~county_code)
mod5 <- feols(long_y ~ Treat + .[covs], data = reg_data_2013, weights = ~set_wt, cluster = ~county_code)
mod6 <- feols(long_y ~ Treat + .[covs], data = reg_data_change, weights = ~set_wt, cluster = ~county_code)

# export table 3
modelsummary(list("(1)" = mod1, "(2)" = mod2, "(3)" = mod3, 
                  "(4)" = mod4, "(5)" = mod5, "(6)" = mod6),
             coef_map = dict, coef_omit = "(Intercept)", escape = FALSE, gof_map = NA,
             fmt = 2, estimate = "{estimate}",
             title = "Regression 2 X 2 DiD with Covariates", 
             output = 'kableExtra'
             ) %>%
  add_header_above(c(" " = 1, "No Covs" = 1, "$$X_{i, t = 2013}$$" = 1, "$$X_{i, t}$$" = 1,
                     "No Covs" = 1, "$$X_{i, t = 2013}$$" = 1, "$$X_{i, t}$$" = 1), 
                   escape = FALSE, extra_css = 'vertical-align: middle !important;', line = F) %>% 
  add_header_above(c(" " = 1, "Unweighted" = 3, "Weighted" = 3), escape = FALSE)

## Table 6: Outcome Regression and Propensity Score Models ----
## Next we make Table 6 which shows the pscore and outcome models that feed into CS.
# These are done by regressing long y on the covariates for the untreated units (the outcome model)
# and regressing the expansion indicator on the covariates for 2013 data (the propensity model) 
# We do it with and without weights
mod1 <- feols(long_y ~ .[covs], data = reg_data_2013 %>% filter(Treat == 0), cluster = ~county_code)
mod2 <- feglm(Treat ~ .[covs], data = short_data %>% filter(year == 2013), family = "binomial", vcov = "hetero")
mod3 <- feols(long_y ~ .[covs], data = reg_data_2013 %>% filter(Treat == 0), cluster = ~county_code, weights = ~set_wt)
mod4 <- feglm(Treat ~ .[covs], data = short_data %>% filter(year == 2013), family = "binomial", vcov = "hetero", weights = ~set_wt)

# This is the dictionary to map variables to data labels for the table
dict <- c("(Intercept)" = "Constant", 
          "perc_female" = "% Female",
          "perc_white" = "% White",
          "perc_hispanic" = "% Hispanic",
          "crude_rate_20_64" = "Crude Mortality Rate",
          "unemp_rate" = "Unemployment Rate",
          "poverty_rate" = "Poverty Rate",
          "median_income" = "Median Income")

# export table 3
modelsummary(list("(1)" = mod1, "(2)" = mod2, "(3)" = mod3, "(4)" = mod4),
             coef_map = dict, escape = FALSE, gof_map = NA,
             fmt = 2, estimate = "{estimate}",
             title = "Outcome Regression and Propensity Score Models", 
             output = 'kableExtra') %>%
  add_header_above(c(" " = 1, "Regression" = 1, "Propensity Score" = 1, 
                     "Regression" = 1, "Propensity Score" = 1), 
                   escape = FALSE, extra_css = 'vertical-align: middle !important;', line = F) %>% 
  add_header_above(c(" " = 1, "Unweighted" = 2, "Weighted" = 2), escape = FALSE)

## RA, IPW, and DR DiD ----
# finally, get the same estimates using Sant'Anna and Zhao (2020) and Callaway and Sant'Anna (2021) using the 
# three adjustment methods
# You need to reformat the group variable (untreated = 0) and the unit ID variable needs to be numeric
data_cs <- short_data %>% 
  mutate(treat_year = if_else(yaca == 2014 & !is.na(yaca), 2014, 0),
         county_code = as.numeric(county_code))

# create a function to run the CS estimator allowing the 
# estimation method to differ
run_cs <- function(method, wt) {
  
  # estimate the att_gt
  atts <- att_gt(
    yname = "crude_rate_20_64",
    tname = "year",
    idname = "county_code",
    gname = "treat_year",
    xformla =  as.formula(paste("~", paste(covs, collapse = "+"))),
    data = data_cs,
    panel = TRUE,
    control_group = "nevertreated",
    bstrap = TRUE,
    cband = TRUE,
    est_method = method,
    weightsname = wt,
    # faster_mode = TRUE,
    base_period = "universal",
    biters = 25000
  )
  
  # aggregate estimates 
  aggte(atts, na.rm = TRUE, biters = 25000) %>% 
    broom::tidy() %>% 
    filter(group == 2014) %>% 
    mutate(type = method)
  
}

# run it three ways - regression adjustment, IPW, and doubly robust
# these are unweighted
ests <- map_dfr(c("reg", "ipw", "dr"), run_cs, wt = NULL)

# make a table - Panel A is with unweighted models
tablea <- ests %>% 
  # reformat the data
  select(type, estimate, std.error) %>% 
  # format the estimate and std error
  mutate(estimate = scales::number(estimate, accuracy = 0.01),
         std.error = paste0("(", scales::number(std.error, accuracy = 0.01), ")")) %>% 
  # reshape the data
  pivot_longer(cols = -type,
               names_to = "statistic",
               values_to = "value") %>% 
  pivot_wider(id_cols = "statistic",
              names_from = "type",
              values_from = "value") %>% 
  mutate(statistic = if_else(statistic == "estimate", "Medicaid Expansion", " "))

# make weighted results for the three CS models.
ests_w <-  map_dfr(c("reg", "ipw", "dr"), run_cs, wt = "set_wt")

# Panel B is the same thing but with weighted results
tableb <- ests_w %>% 
  select(type, estimate, std.error) %>% 
  mutate(estimate = scales::number(estimate, accuracy = 0.01),
         std.error = paste0("(", scales::number(std.error, accuracy = 0.01), ")")) %>% 
  pivot_longer(cols = -type,
               names_to = "statistic",
               values_to = "value") %>% 
  pivot_wider(id_cols = "statistic",
              names_from = "type",
              values_from = "value") %>% 
  select(reg, ipw, dr)

# Combine Panels A and B together and format the table
bind_cols(tablea, tableb) %>% 
  kable(col.names = c(" ", "Regression", "IPW", "Doubly Robust",
                      "Regression", "IPW", "Doubly Robust"),
        align = 'lcccccc',
        escape = F,
        booktabs = T,
        label = "2x2_csdid",
        caption = "Callaway and Sant'Anna (2021) DiD",
        linesep = "") %>% 
  kable_styling(latex_options = c("hold_position")) %>% 
  add_header_above(c(" " = 1, "Unweighted" = 3, "Weighted" = 3))

## Figure 1: Propensity Score Distributions ----
# Finally, we make Figure 1 which reports the distribution of the propensity scores between 
# expansion and non-expansion counties.
# first add in propensity scores to the data
plot_data <- bind_rows(
  short_data %>% 
    filter(year == 2013) %>% 
    mutate(propensity = predict(mod2, ., type = "response"),
           mod = "Unweighted",
           wt = 1),
  short_data %>% 
    filter(year == 2013) %>% 
    mutate(propensity = predict(mod4, ., type = "response"),
           mod = "Weighted", 
           wt = set_wt)
)

# plot the propensity score distributions by group
plot_data %>% 
  mutate(expand = if_else(Treat == 1, "Expansion Counties", "Non-Expansion Counties")) %>% 
  ggplot(aes(x = propensity, y = after_stat(density), group = expand, color = expand, weight = wt)) + 
  geom_histogram(fill = "white", position = "identity", linewidth = 1, alpha = 0.5) + 
  facet_wrap(~mod) + 
  scale_color_brewer(palette = 'Set1') + 
  labs(x = "Propensity Score", y = "Density") + 
  theme(legend.position = 'bottom',
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.text = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.title = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))


# 2 x T ----
# set seed for reproducibility
set.seed(20240924)

# load cleaned data
mydata <- read_csv(here::here("data", "county_mortality_data.csv")) %>% 
  # make state the abbreviation
  mutate(state = str_sub(county, nchar(county) - 1, nchar(county))) %>% 
  # drop DC and pre-2014 adoption states
  filter(!(state %in% c("DC", "DE", "MA", "NY", "VT"))) %>% 
  # drop states that adopt between 2014 and 2019
  filter(yaca == 2014 | is.na(yaca) | yaca > 2019)

# set the covariates that we're going to use.
covs <- c("perc_female","perc_white", "perc_hispanic", 
          "unemp_rate", "poverty_rate", "median_income")

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

# the pre-processing steps above are exactly the same as in 2 X 2, though the last step is only necessary here in 2 X T, because in 2 X 2 we only need to observe 2013 and 2014.

# make ACA and post variables, as well as the weighting variable which is the relevant county 
# population in 2013
mydata <- mydata %>% 
  mutate(Treat = if_else(yaca == 2014 & !is.na(yaca), 1, 0),
         Post = if_else(year >= 2014, 1, 0)) %>% 
  group_by(county_code) %>% 
  # make a variable with population weight in 2013
  mutate(set_wt = population_20_64[which(year == 2013)]) %>% 
  ungroup()
# the only difference from short_data in 2 X 2 is that we keep all years, not just 2013 and 2014.

## Figure 2: County Mortality Trends by Expansion Decision ----
# make time series plot
mydata %>% 
  mutate(expand = if_else(Treat == 1, "Expansion Counties", "Non-Expansion Counties")) %>% 
  # get the weighted averages by expansion type and year
  group_by(expand, year) %>% 
  summarize(mortality = weighted.mean(crude_rate_20_64, set_wt)) %>% 
  # plot
  ggplot(aes(x = year, y = mortality, group = expand, color = expand)) + 
  geom_point(size = 2) + geom_line(linewidth = 1) + 
  geom_vline(xintercept = 2014, linetype = "dashed") + 
  scale_color_brewer(palette = 'Set1') + 
  scale_x_continuous(breaks = 2009:2019) + 
  labs(x = "", y = "Mortality (20-64) \n Per 100,000") + 
  theme(legend.position = 'bottom',
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.text = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.title = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))


## Figure 3: 2 X T Event Study ----
# Make event study plots
# first add in the variables needed to run the CS model - timing group variable (treat_year)
# and county code needs to be numeric
mydata <- mydata %>% 
  mutate(treat_year = if_else(yaca == 2014 & !is.na(yaca), 2014, 0),
         county_code = as.numeric(county_code),
         time_to_treat = if_else(Treat == 1, year - treat_year, 0))

# estimate CS models

# get the individual ATT(g,t) estimates
mod <- did::att_gt(
  yname = "crude_rate_20_64",
  tname = "year",
  idname = "county_code",
  gname = "treat_year",
  xformla =  NULL,
  data = mydata,
  panel = TRUE,
  control_group = "nevertreated",
  bstrap = TRUE,
  cband = TRUE,
  est_method = "reg",
  weightsname = "set_wt",
  # faster_mode = TRUE,
  base_period = "universal",
  biters = 25000
)

# confirm you get the same with OLS (standard errors differ because of bootstrap only)
cs_out <- mod

ols_out <- feols(crude_rate_20_64 ~ i(time_to_treat, Treat, ref = -1) | county_code + year, 
                 data = mydata, 
                 cluster = ~county_code, 
                 weights = ~set_wt)

# get the Rambachan/Roth confidence interval
# first save our aggregate event study estimates
es <- did::aggte(
  mod,
  type = "dynamic",
  min_e = -5,
  max_e = 0,
  bstrap = TRUE,
  biters = 25000
)

# source the honest_did code made to fit our estimates
source(here::here('scripts/R/5_honestdid.R'))

# get robust CI for ATT(2014), the treatment effect in the first year of treatment for those counties that adopted Medicaid expansion.
robust_ci <- honest_did(es = es, type = "relative_magnitude")$robust_ci

# get the aggregate value for e = 0:5
agg <- mod %>% 
  aggte(type = "dynamic", min_e = 0, max_e = 5,
        bstrap = TRUE, biters = 25000)

# make labels for the estimate, std error, and confidence intervals
label1 <-  paste0("Estimate~(e %in% '{0, 5}')~'='~'", scales::number(agg$overall.att, accuracy = 0.01), "'")

label2 <- paste0("Std. Error = ", scales::number(agg$overall.se, 0.01), " \n",
                 "Conf. Int = [", scales::number(agg$overall.att - 1.96*agg$overall.se, 0.01), ", ", 
                 scales::number(agg$overall.att + 1.96*agg$overall.se, 0.01), "]")

# make event study plot for Figure 3
mod %>% 
  # Aggregate in event time ("dynamic")
  aggte(type = "dynamic", biters = 25000) %>% 
  # get the two confidence intervals
  broom::tidy(conf.int = TRUE) %>% 
  # plot
  ggplot(aes(x = event.time, y = estimate)) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), color = "darkred") + 
  geom_linerange(aes(ymin = point.conf.low, ymax = point.conf.high)) + 
  geom_point() + 
  geom_vline(xintercept = -1, linetype = "dashed") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_x_continuous(breaks = -5:5) + 
  annotate("text", x = 3, y = 11, label = label1, parse = TRUE) + 
  annotate("text", x = 3, y = 9, label = label2) + 
  labs(x = "Event Time", y = "Treatment Effect \n Mortality Per 100,000") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.text = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.title = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# get robust CI
robust_ci %>% 
  # keep just relative magnitude = 1
  filter(Mbar == 1) %>%
  # drop extraneous variables
  select(lb, ub, Mbar) %>% 
  # round
  mutate_at(c("lb", "ub"), scales::number, accuracy = 0.01) %>% 
  # make table
  kable(col.names = c("lower_bound", "upper_bound", "relative_magnitude"),
        align = 'c', booktabs = T) %>% 
  kable_styling()

## Figure 4: 2 X T Event Study With Covariates ----
# Estimate the CS Model 3 ways with the three estimation types
# make a function to run CS, varying the estimation type
run_cs <- function(method) {
  
  # get the ATT(g, t) estimates
  att_gt(
    yname = "crude_rate_20_64",
    tname = "year",
    idname = "county_code",
    gname = "treat_year",
    xformla =  as.formula(paste("~", paste(covs, collapse = "+"))),
    data = mydata,
    panel = TRUE,
    control_group = "nevertreated",
    bstrap = TRUE,
    cband = TRUE,
    est_method = method,
    weightsname = "set_wt",
    # faster_mode = TRUE,
    base_period = "universal", 
    biters = 25000
  ) %>% 
    # aggregate to event time (dynamic)
    aggte(type = "dynamic", na.rm = TRUE, biters = 25000) %>% 
    broom::tidy(conf.int = TRUE) %>% 
    mutate(type = method)
}

# estimate it for 3 methods 
out <- map_dfr(c("reg", "ipw", "dr"), run_cs)

# make plot for Figure 4 - 2XT Event Study with Covariates
out %>% 
  # refactor labels for plot
  mutate(type = case_match(type,
                           "reg" ~ "Regression",
                           "ipw" ~ "IPW",
                           "dr" ~ "Doubly Robust")) %>% 
  mutate(type = factor(type, levels = c("Regression", "IPW", "Doubly Robust"))) %>% 
  # plot the esti mates and confidence intervals
  ggplot(aes(x = event.time, y = estimate)) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), color = "darkred") + 
  geom_linerange(aes(ymin = point.conf.low, ymax = point.conf.high)) + 
  geom_point() + 
  geom_vline(xintercept = -1, linetype = "dashed") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  facet_wrap(~type) + 
  scale_x_continuous(breaks = -5:5) + 
  labs(x = "Event Time", y = "Treatment Effect \n Mortality Per 100,000") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.text = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.title = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# G x T ----
## Figure 5: Mortality Trends by Expansion Decision with Staggered Timing ----
# make time series plot by timing group (Figure 5)
# set seed for reproducibility
set.seed(20240924)

# load cleaned data
mydata <- read_csv(here::here("data", "county_mortality_data.csv")) %>% 
  # make state the abbreviation
  mutate(state = str_sub(county, nchar(county) - 1, nchar(county))) %>% 
  # drop DC and pre-2014 adoption states
  filter(!(state %in% c("DC", "DE", "MA", "NY", "VT"))) 

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

# make smaller dataset
mydata <- mydata %>% 
  mutate(Treat = if_else(!is.na(yaca) & yaca <= 2019, 1, 0),
         treat_year = if_else(!is.na(yaca) & yaca <= 2019, yaca, 0),
         Post = if_else(year >= 2014, 1, 0)) %>% 
  group_by(county_code) %>% 
  # make a variable with population weight in 2013
  mutate(set_wt = population_20_64[which(year == 2013)]) %>% 
  ungroup()

# make time series plot by timing group (Figure 5)
mydata %>% 
  # identify the groups
  mutate(treat_year = if_else(treat_year == 0, "Non-Expansion Counties", as.character(treat_year))) %>% 
  # get weighted average mortality by gtiming-group and year
  group_by(treat_year, year) %>% 
  summarize(mortality = weighted.mean(crude_rate_20_64, set_wt)) %>% 
  ggplot(aes(x = year, y = mortality, group = as.factor(treat_year), color = as.factor(treat_year))) + 
  geom_point(size = 2) + 
  geom_line(linewidth = 1) + 
  scale_color_manual(values = c("#7C7189", "#D04E59", "#BC8E7D", "#2F3D70", "#CABEE9")) + 
  scale_x_continuous(breaks = 2009:2019) + 
  labs(x = "", y = "Mortality (20-64) \n Per 100,000") + 
  theme(legend.position = 'bottom',
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.text = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.title = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))


## Figure 6: ATT(g, t)s for Each Expansion Group ----
# reclassify the county code as numeric for Callaway/Sant'Anna
mydata <- mydata %>% 
  mutate(county_code = as.numeric(county_code))

# get the ATT(g,t)'s
mod_no_x <- att_gt(
  yname = "crude_rate_20_64",
  tname = "year",
  idname = "county_code",
  gname = "treat_year",
  xformla =  NULL,
  data = mydata,
  panel = TRUE,
  control_group = "notyettreated",
  bstrap = TRUE,
  biters = 25000,
  cband = TRUE,
  weightsname = "set_wt",
  # faster_mode = TRUE,
  base_period = "universal"
)

# make plots in calendar time by timing group
mod_no_x %>% 
  # get the estimates and confidence intervals
  broom::tidy(conf.int = TRUE) %>% 
  mutate(group = as.character(group)) %>% 
  # plot the estimates and CIs
  ggplot(aes(x = time, y = estimate, color = group)) + 
  geom_point(size = 2) + geom_line(linewidth = 1) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) + 
  scale_x_continuous(breaks = seq(2009, 2019, by = 2),
                     labels = seq(2009, 2019, by = 2)) + 
  scale_color_manual(values = c("#7C7189", "#D04E59", "#BC8E7D", "#2F3D70")) + 
  geom_vline(aes(xintercept = as.numeric(group) - 1), linetype = "dashed", linewidth = 1) + 
  labs(x = "", y = "Treatment Effect \n Mortality Per 100,000") + 
  facet_wrap(~group) + 
  theme(legend.position = 'bottom',
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.text = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.title = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))


## Figure 7: ATT(g, t) in Event Time ----
# Make a different plot that reports the ATT(g,t)s in relative time
mod_no_x %>% 
  # get the estimates and CIs
  broom::tidy() %>% 
  # define relative time for each group
  mutate(rel_time = time - group) %>% 
  # plot
  ggplot(aes(x = rel_time, y = estimate, group = as.factor(group), color = as.factor(group))) + 
  geom_point(size = 2) + geom_line(linewidth = 1) + 
  geom_vline(xintercept = -1, linetype = "dashed", linewidth = 1) + 
  scale_color_manual(values = c("#7C7189", "#D04E59", "#BC8E7D", "#2F3D70")) + 
  scale_x_continuous(breaks = -10:5) + 
  labs(x = "", y = "Treatment Effect \n Mortality Per 100,000") + 
  theme(legend.position = 'bottom',
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.text = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.title = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

## Figure 8: G x T Event Study Without Covariates ----
# Make a full -5 to + 5 event study plot for no covariates 
# using Callaway/Sant'Anna with not-yet-treated

# get the aggregate value for e = 0:5
agg <- mod_no_x %>% 
  aggte(type = "dynamic", min_e = 0, max_e = 5,
        bstrap = TRUE, biters = 25000)

# make labels for the estimate, std error, and confidence intervals
label1 <-  paste0("Estimate~(e %in% '{0, 5}')~'='~'", scales::number(agg$overall.att, accuracy = 0.01), "'")

label2 <- paste0("Std. Error = ", scales::number(agg$overall.se, 0.01), " \n",
                 "Conf. Int = [", scales::number(agg$overall.att - 1.96*agg$overall.se, 0.01), ", ", 
                 scales::number(agg$overall.att + 1.96*agg$overall.se, 0.01), "]")

# make G*T event study plot without covariates
mod_no_x %>% 
  # aggregate into relative time (dynamic)
  aggte(type = "dynamic") %>% 
  broom::tidy(conf.int = TRUE) %>% 
  # keep just -5 to + 5
  filter(event.time %>% between(-5, 5)) %>% 
  # plot
  ggplot(aes(x = event.time, y = estimate)) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), color = "darkred") + 
  geom_linerange(aes(ymin = point.conf.low, ymax = point.conf.high)) + 
  geom_point() + 
  geom_vline(xintercept = -1, linetype = "dashed") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_x_continuous(breaks = -5:5) + 
  annotate("text", x = 3, y = 11, label = label1, parse = TRUE) + 
  annotate("text", x = 3, y = 9, label = label2) + 
  labs(x = "Event Time", y = "Treatment Effect \n Mortality Per 100,000") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.text = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.title = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

## Figure 9: G x T Event Study With Covariates ----
# Now re-estimate and plot the same GXT estimates with covariates
# set.seed(20240924)
mod_with_x <- att_gt(
  yname = "crude_rate_20_64",
  tname = "year",
  idname = "county_code",
  gname = "treat_year",
  xformla =  as.formula(paste("~", paste(covs, collapse = "+"))),
  data = mydata,
  panel = TRUE,
  control_group = "notyettreated",
  bstrap = TRUE,
  biters = 25000,
  cband = TRUE,
  est_method = "dr",
  weightsname = "set_wt",
  # faster_mode = TRUE,
  base_period = "universal"
  
)

# get the aggregate value for e = 0:5
agg <- mod_with_x %>% 
  aggte(type = "dynamic", min_e = 0, max_e = 5,
        bstrap = TRUE, biters = 25000)

# make labels for the estimate, std error, and confidence intervals
label1 <-  paste0("Estimate~(e %in% '{0, 5}')~'='~'", scales::number(agg$overall.att, accuracy = 0.01), "'")

label2 <- paste0("Std. Error = ", scales::number(agg$overall.se, 0.01), " \n",
                 "Conf. Int = [", scales::number(agg$overall.att - 1.96*agg$overall.se, 0.01), ", ", 
                 scales::number(agg$overall.att + 1.96*agg$overall.se, 0.01), "]")

# make event study plot for GXT with covariates
mod_with_x %>% 
  # aggregate to event time (dynamic)
  aggte(type = "dynamic") %>% 
  broom::tidy(conf.int = TRUE) %>% 
  # filter years
  filter(event.time %>% between(-5, 5)) %>% 
  # plot
  ggplot(aes(x = event.time, y = estimate)) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), color = "darkred") + 
  geom_linerange(aes(ymin = point.conf.low, ymax = point.conf.high)) + 
  geom_point() + 
  geom_vline(xintercept = -1, linetype = "dashed") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_x_continuous(breaks = -5:5) + 
  annotate("text", x = 3, y = 18, label = label1, parse = TRUE) + 
  annotate("text", x = 3, y = 15, label = label2) + 
  labs(x = "Event Time", y = "Treatment Effect \n Mortality Per 100,000") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.text = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.title = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))
