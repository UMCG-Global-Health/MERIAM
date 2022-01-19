library(checkpoint)
checkpoint("2020-06-22", checkpointLocation = getwd())

#library(eurostat)
library(dplyr)
library(tibble)
library(lubridate)
library(forcats)
library(tidyr)
library(stringr)
library(incidence)
library(aweek)
library(OECD)
library(countrycode)
library(readr)
library(purrr)
library(magrittr)

incidence <- read_csv("data_input/who/ariili.csv", 
                          col_types = cols(SDATE = col_date(format = "%Y-%m-%d"), 
                                                     EDATE = col_date(format = "%Y-%m-%d"), 
                                                     AGEGROUP_CODE = col_skip(), ILI_OUTPATIENTS = col_skip(), 
                                                     ILI_OUT_RATE = col_skip(), ILI_SAMPLED = col_skip(), 
                                                     SARI_CASES = col_skip()), skip = 3)

incidence %<>% select(Country, Year, Week, ILI_POP_RATE, ARI_POP_RATE, SDATE) %>%
  mutate(total = if_else(is.na(ILI_POP_RATE), 0, ILI_POP_RATE) + if_else(is.na(ARI_POP_RATE), 0, ARI_POP_RATE),
         country = str_to_lower(countrycode(Country, "country.name", "iso2c"))) %>%
  filter(country %in% c("be", "de", "nl", "gr", "pl", "am"),
         Week < 21 | Week > 39) %>%
  rename(ili = ILI_POP_RATE, ari = ARI_POP_RATE, date = SDATE) %>%
  mutate(iso_week = get_aweek(Week, year = Year),
         season = if_else(Week > 39, str_c(Year, "-", Year+1), str_c(Year-1, "-", Year))) %>%
  arrange(date)

pivot_incidence <- incidence %>%
  pivot_longer(cols = c("ari","ili"),
               names_to = "type",
               values_to = "incidence",
               values_drop_na = TRUE)


  
model_input <- pivot_incidence %>%
  group_by(country, season, type) %>%
  nest()

model_output <- model_input %>%
  mutate(object = map(data, ~as.incidence(.x$total, dates = .x$date)),
         model = map(object, fit_optim_split),
         plot = map2(object, model, ~plot(.x, fit = .y$fit)))


saveRDS(model_output, file = "data_input/who/models.RDS")


