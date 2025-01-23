# Exploring the data for the R Bootcamp

set.seed(122) # I'm selecting countries randomly below

# Loading libraries -------------------------------------------------------

library(tidyverse)

# Reading the data --------------------------------------------------------

data_env <- read_csv("data_env.csv")
data_std <- read_csv("data_std.csv") # also use this dataset to get categorical variables

# Creating vectors with variables of interest -----------------------------

# My criteria for selecting variables were:
# - Get numerical and categorical variables
# - Get variables with missing values, but with enough data to work with
# - Get a decent amount of variables, but not too many to keep it manageable
# - Not too technical so that people from different disciplines can understand
# - Interesting

# Standard Dataset

id_variables_std <- c(
  "cname_qog", # country name p. 59
  "year" # year p. 60
)

categorical_variables_std <- c(
  "br_elect", # degree of multi-party competition ~1990-2020 p. 183
  "ht_region" # politico-geographic classification of world regions 1990-2020 p. 1267
)

variables_std <- c(id_variables_std, categorical_variables_std)

# Environmental Indicators Dataset

id_variables_env <- c(
  "cname_qog", # country name p. 28
  "year" # year p. 28
)

categorical_variables_env <- c(
  "act_act" # whether a country has an accountable climate target or not 2015 p. 30
)

numeric_variables_env <- c(
  "cckp_rain", # annual average rainfall ~1950-2020 p.34
  "cckp_temp", # annual average temperature ~1950-2020 p. 35
  "wdi_co2", # co2 emissions per capita p. 300
  "fao_luagr", # agricultural land as % of total land area 1960-2020 p. 110
  "fao_luforest", # forest land as a share of total land area 1990-2020 p. 115
  "epi_cda", # CO2 growth rate ~1995-2020 p. 129
  "epi_msw", # proportion of waste treated in a manner that controls environmental risks ~1995-2020 p. 141
  "epi_uwd" # unsafe drinking water ~1995-2020 p. 151
)

variables_env <- c(id_variables_env, categorical_variables_env, numeric_variables_env)

# Subset variables and years ----------------------------------------------

# I'm focusing on 1990-2020, where most of the variables have data
# It's ok to have missing values, though, since that's part of the workshops

data_std <- data_std %>%
  select(all_of(variables_std)) %>% 
  filter(between(year, 1990, 2020))

data_env <- data_env %>%
  select(all_of(variables_env)) %>% 
  filter(between(year, 1990, 2020))

# Now I don't want id vars in the vectors
variables_std <- c(categorical_variables_std)
variables_env <- c(categorical_variables_env, numeric_variables_env)

# Explore missing values by country --------------------------------------------------

# I want to make sure we have enough data but there are still missing data (but not too complex)

# countries for which at least one of the variables has half or more missing
data_std %>% 
  group_by(cname_qog) %>% 
  summarise(
    across(
      all_of(variables_std),
      ~ mean(is.na(.)),
      .names = "na_prop_{.col}"
    )
  ) %>% #View()
  rowwise() %>%
  filter(any(c_across(starts_with("na_prop_")) >= 0.5)) %>%
  pull(cname_qog)
# 11 countries, that's ok

# countries for which at least one of the variables has half or more missing (except act_act)
data_env %>% 
  group_by(cname_qog) %>% 
  summarise(
    across(
      all_of(variables_env),
      ~ mean(is.na(.)),
      .names = "na_prop_{.col}"
    )
  ) %>% #View()
  select(-na_prop_act_act) %>% # this one we have only for 2015 and that's ok
  rowwise() %>%
  filter(any(c_across(starts_with("na_prop_")) >= 0.5)) %>% 
  pull(cname_qog)
# 20 countries, that's ok

# countries for which, in 2015, act_act is missing
data_env %>% 
  filter(year == 2015 & is.na(act_act)) %>% 
  pull(cname_qog)
# 11 countries, that's ok
  
# that's good to leave some missing values

# Explore missing values by year ------------------------------------------

# years for which all of the variables have less than half missing
data_std %>% 
  group_by(year) %>% 
  summarise(
    across(
      all_of(variables_std),
      ~ mean(is.na(.)),
      .names = "na_prop_{.col}"
    )
  ) %>% #View()
  rowwise() %>%
  filter(all(c_across(starts_with("na_prop_")) < 0.5)) %>% 
  pull(year)
# all years (but there are still some missing data)

# years for which all of the variables have less than half missing (except act_act)
data_env %>% 
  group_by(year) %>% 
  summarise(
    across(
      all_of(variables_env),
      ~ mean(is.na(.)),
      .names = "na_prop_{.col}"
    )
  ) %>% #View()
  select(-na_prop_act_act) %>%
  rowwise() %>%
  filter(all(c_across(starts_with("na_prop_")) < 0.5)) %>% 
  pull(year)
# between 1995 and 2016 (which still have some missing data)

# although we could subset the data to 1995 - 2016, it's probably ok (and good) to leave some more missing data

# Join datasets -----------------------------------------------------------

# inner_join is fine. it's not super important for our purposes anyways
data <- inner_join(data_std, data_env, by = c("cname_qog", "year"))

# Are there enough interesting patterns? ----------------------------------

# Just getting a quick sense of whether there are interesting patterns for the exercises, particularly data viz on day 4

# getting a sample of countries to visualize
countries_sample <- data %>%
  distinct(cname_qog) %>%
  sample_n(30) %>%
  pull(cname_qog)

# co2 over time
data %>%
  filter(cname_qog %in% countries_sample) %>%
  ggplot(aes(x = year, y = wdi_co2)) +
  geom_line() +
  facet_wrap(~ cname_qog, scales = "free_y")
# some going up, some down. that;s interesting

# co2 growth rate over time
data %>%
  filter(cname_qog %in% countries_sample) %>%
  ggplot(aes(x = year, y = epi_cda)) +
  geom_line() +
  facet_wrap(~ cname_qog, scales = "free_y")
# more complex than up or down, could be interesting?

# temperature over time
data %>%
  filter(cname_qog %in% countries_sample) %>%
  ggplot(aes(x = year, y = cckp_temp)) +
  geom_line() +
  facet_wrap(~ cname_qog, scales = "free_y")
# up across. that's interesting

# co2 over time by region
data %>% 
  group_by(ht_region, year) %>% 
  summarise(mean_wdi_co2 = mean(wdi_co2, na.rm = T)) %>% 
  ggplot(aes(x = year, y = mean_wdi_co2, color = factor(ht_region))) +
  geom_line() 
# not the most interesting?

# co2 over time by degree of multi-party competition
data %>% 
  group_by(br_elect, year) %>% 
  summarise(mean_wdi_co2 = mean(wdi_co2, na.rm = T)) %>% 
  ggplot(aes(x = year, y = mean_wdi_co2, color = factor(br_elect))) +
  geom_line() 
# some up, some down. interesting

# Subset data to 2015 -----------------------------------------------------

# for act_act and, mostly, to get an easier version of the data for the first exercises

data_2015 <- filter(data, year == 2015)

colMeans(is.na(data_2015))
# still some NAs, but very little

# Save data ---------------------------------------------------------------

write_csv(data, "data.csv")

write_csv(data_2015, "data_2015.csv")
