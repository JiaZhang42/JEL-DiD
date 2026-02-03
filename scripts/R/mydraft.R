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