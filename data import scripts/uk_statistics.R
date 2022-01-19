# script is used to get UK demographic data in Eurostat format

library(checkpoint)
checkpoint("2021-03-01", checkpointLocation = getwd())

library(dplyr)
library(lubridate)
library(forcats)
library(tidyr)
library(stringr)
library(purrr)
library(incidence)
library(aweek)
library(countrycode)
library(future.apply)
library(readxl)
library(eurostat)
source("functions/stats.R") #load population functions

import_file <- "data_input/uk_data/uk_ppp_opendata2018.xlsx"


# Base population projections ---------------------------------------------


population <- read_xlsx(import_file, sheet = "Population")

pop_uk_agecats <- population %>%
  pivot_longer(cols = !c("Sex", "Age"), names_to = "time", values_to = "values") %>%
  mutate(sex = case_when(Sex == 1 ~ "M",
                         Sex == 2 ~ "F"),
         geo = "gb",
         unit = "PER",
         projection = "BSL") %>%
  select(projection, unit, sex, age = Age, geo, time, values)

pop_uk_totalsex <- pop_uk_agecats %>%
  group_by(projection, unit, age, geo, time) %>%
  summarise(values = sum(values),
            sex = "T",
            .groups = "drop") %>%
  select(projection, unit, sex, age, geo, time, values)

pop_uk_totalage <- pop_uk_agecats %>%
  bind_rows(pop_uk_totalsex) %>%
  group_by(projection, unit, sex, geo, time) %>%
  summarise(values = sum(values),
            age = "Total",
            .groups = "drop") %>%
  select(projection, unit, sex, age, geo, time, values)

pop_uk <- pop_uk_agecats %>%
  bind_rows(pop_uk_totalsex) %>%
  mutate(age = str_c("Y",age),
         age = case_when(age == "Y0" ~ "Y_LT1",
                   age == "Y100" ~ "Y_GE100",
                   age == "Y101" ~ "Y_GE100",
                   age == "Y102" ~ "Y_GE100",
                   age == "Y103" ~ "Y_GE100",
                   age == "Y104" ~ "Y_GE100",
                   age == "Y105 - 109" ~ "Y_GE100",
                   age == "Y110 and over" ~ "Y_GE100",
                   is.character(age) ~ age)) %>%
  group_by(projection, unit, sex, age, geo, time) %>%
  summarise(values = sum(values),
            .groups = "drop") %>%
  bind_rows(pop_uk_totalage) %>%
  mutate(time = as.double(time))

saveRDS(pop_uk, "data_input/generated/data_uk/pop_proj.RDS")


# Mortality ---------------------------------------------------------------

#read in number of deaths
mor_uk_deathnum <- read_xlsx(import_file, sheet = "Deaths") %>%
  pivot_longer(cols = !c("Sex", "Age"), names_to = "time", values_to = "deathnum") %>%
  filter(!(Age %in% c("Birth", "105+"))) %>%
  mutate(time = as.double(str_sub(time, 8, 11)),
         sex = case_when(Sex == 1 ~ 0,
                         Sex == 2 ~ 1),
         age_num = as.double(Age)) %>%
  filter(age_num < 100) %>%
  mutate(age_cat = case_when(age_num == 0 ~ "Y_LT1",
                             is.double(age_num) ~  str_c("Y",age_num)),
         geo = "gb") %>%
  select(geo, sex, age_num, age_cat, time, deathnum)

#convert to death rate
mor_uk_deathrate <- pop_uk %>%
  filter(age != "Y_GE100",
         sex != "T",
         time != 2018) %>%
  mutate(sex = if_else(sex == "F", 1, 0)) %>%
  select(sex, age, time, total_pop = values) %>%
  left_join(mor_uk_deathnum, by = c("sex", "age" = "age_cat", "time")) %>%
  mutate(values = deathnum / total_pop) %>%
  select(sex, age = age_num, geo, time, values)
  
#correct mortality data for mortality from influenza and pneumonia
#using standardized death rate per 100,000 inhabitants from eurostat
dis_data <- get_eurostat(id = "hlth_cd_asdr", time_format = "num") %>%
  select(sex, age, icd10, geo, time, values) %>%
  filter(sex != "T",
         icd10 == "A-R_V-Y" | icd10 ==  "J09-J11" | icd10 == "J12-J18", #all causes, influenza (inc swine flu), pneumonia
         time == 2010,#most recent
         age == "Y_GE65" | age == "Y_LT65")  %>% #age 65 and over, age younger then 65
  mutate(over65 = ifelse(age == "Y_GE65", 1, 0),
         sex = ifelse(sex == "M", 0, 1),
         geo = str_to_lower(countrycode(geo, origin = "eurostat", destination = "iso2c"))) %>%
  ggplot2::remove_missing(vars = "geo") %>%
  pivot_wider(names_from = icd10, values_from = values, values_fill = 0) %>%
  mutate(death_proportion = (`J09-J11` +`J12-J18`) /`A-R_V-Y`)

#calculate correction factor based on age and sex
correct_mortality_factor <- function(in_geo, in_sex, in_age, cause_of_death_data){
  correction_factor <- cause_of_death_data %>%
    filter(sex == in_sex,
           over65 == ifelse(in_age < 65, 0, 1),
           geo == in_geo) %>%
    pull(death_proportion)
  
  correction_factor
}

plan(multisession)

correction_factors <- future_mapply(correct_mortality_factor, mor_uk_deathrate$geo, mor_uk_deathrate$sex, mor_uk_deathrate$age, MoreArgs = list(cause_of_death_data = dis_data))

plan(sequential)

mor_uk_corrected <- mor_uk_deathrate %>% 
  group_by(geo, sex, age) %>% 
  nest() %>%
  mutate(cor_factor = pmap(list(geo, sex, age), ~dis_data %>% filter(geo == ..1, sex == ..2, over65 == ifelse(..3 < 65, 0, 1)) %>% pull(death_proportion)),
         cor_factor = map_dbl(cor_factor, ~ifelse(length(.x) > 0, .x, NA)),
         cor_data = map2(data, cor_factor, ~.x %>% mutate(values_uncorrected = values, values = values * (1-.y)))) %>%
  select(-data, -cor_factor) %>%
  unnest(cols = "cor_data")

saveRDS(mor_uk_corrected, "data_input/generated/data_uk/mortality_data.RDS")



# Migration ---------------------------------------------------------------

mig_uk <- read_xlsx(import_file, sheet = "International_migration") %>%
  pivot_longer(cols = !c("Flow", "Sex", "Age"), names_to = "time", values_to = "values") %>%
  filter(Age != "105+",
         Flow == "Net") %>%
  mutate(time = as.double(str_sub(time, 8, 11)),
         sex = case_when(Sex == 1 ~ 0,
                         Sex == 2 ~ 1),
         age = as.double(Age),
         geo = "uk")  %>%
  select(geo, sex, age, time, values)

saveRDS(mig_uk, "data_input/generated/data_uk/migration_data.RDS")
  
