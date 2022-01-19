rm(list=ls())
#script to download and store data from various sources for the analysis
checkpoint::checkpoint("2021-07-20", checkpoint_location = getwd())

library(eurostat)
library(dplyr)
library(tibble)
library(lubridate)
library(forcats)
library(tidyr)
library(stringr)
library(tsibble)
library(incidence)
library(aweek)
library(ggplot2)
library(OECD)
library(countrycode)
library(future.apply)
source("functions/stats.R") #load population functions

plan(multiprocess)
#######INPUTS###########
data_dir_prefix <- "data"
countrycode <- "nl"
price_level <- 2019
start_year <- 2020



#######END INPUTS########

#create folder to store in
dir_gen <- paste0(data_dir_prefix,"_") #folder to store data in

#population data (basically population pyramid)
##download population data from eurostat
pop <- get_eurostat(id = "proj_19np", time_format = "num")  %>%
  filter(projection =="BSL") %>%
  mutate(geo = str_to_lower(countrycode(geo, origin = "eurostat", destination = "iso2c"))) %>%
  remove_missing(vars = "geo")

saveRDS(pop, file = data_input/generated/pop_proj.RDS)

##filter for specified year
pop_year <- pop %>% filter(time == start_year)

##create data frame to be used for model
age_pop <- pop_year %>%
  filter(sex == "T") %>%
  select(age, values) %>%
  mutate(ageprop = values/(pop_year$values[205]-pop_year$values[305]),
         age = str_sub(age, start = 2, end = 4),
         age = ifelse(age == "_LT", 0, age)) %>%
  filter(age != "_GE",
         age != "OTA") %>%
  arrange(age) %>%
  mutate(age = as.double(age))
saveRDS(age_pop, file = paste("data_input/generated/", dir_gen, "/pop_pyramid.RDS", sep = ""))


#calculate probability of being female
female_prob <- pop_year$values[1]/pop_year$values[205]
saveRDS(female_prob, file = paste("data_input/generated/", dir_gen, "/female_prob.RDS", sep = ""))

#fertility rate
##create subset of population data
pop_babies <- pop %>% filter(sex == "T", age %in% c("Y_LT1", "TOTAL"))

##simple for loop to calculate for each year the number of newborns using the total population from the previous year
calc_year <- start_year + 1
fertility <- tibble(
  year = calc_year:2100,
  fertility = 0
)
for(i in 1:length(fertility$year)) {
  fertility_value <- as.numeric(filter(pop_babies, time == calc_year, age == "Y_LT1")[,7])/as.numeric(filter(pop, time == calc_year-1, sex == "T", age == "TOTAL")[,7])
  fertility[i,2] <- fertility_value
  calc_year <- calc_year + 1
}

saveRDS(fertility, file = paste("data_input/generated/", dir_gen, "/fertility_rates.RDS", sep = ""))
  



#migration rates
##import data from eurostat, summarise with totals and ratios of females
mig <- get_eurostat(id = "proj_19nanmig", time_format = "num") %>%
  remove_missing(vars = "geo") %>%
  filter(age != "Y_GE100",
         age != "TOTAL",
         sex == "M" | sex == "F",
         projection == "BSL") %>%
  mutate(geo = str_to_lower(countrycode(geo, origin = "eurostat", destination = "iso2c")),
         age = str_sub(age, start = 2, end = 4),
         age = ifelse(age == "_LT", 0, age),
         age = as.double(age),
         sex = ifelse(sex == "M", 0, 1)) %>%
  select(geo, sex, age, time, values) %>%
  arrange(age)


saveRDS(mig, file = "data_input/generated/migration_data.RDS")



##total population to relate migration rates to
t_pop <- pop  %>%
  select(sex, age, time, values) %>%
  mutate(age = str_sub(age, start = 2, end = 4),
         age = ifelse(age == "_LT", 0, age)) %>%
  filter(age == "OTA", sex == "T") 

##use total popilation to relate migration to total

for(i in 1:nrow(mig)) {
  mig$rate[i] <- mig$values[i] / t_pop$values[t_pop$time == mig$time[i]]
}





#mortality rates
mor_data <- get_eurostat(id = "proj_19naasmr", time_format = "num") %>%
  filter(projection == "BSL", age != "Y_GE100") %>%
  mutate(age = str_sub(age, start = 2, end = 4),
         age = ifelse(age == "_LT", 0, age),
         age = as.double(age),
         sex = ifelse(sex == "M", 0, 1),
         geo = str_to_lower(countrycode(geo, origin = "eurostat", destination = "iso2c"))) %>%
  select(-projection, -unit)

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
  remove_missing(vars = "geo") %>%
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


correction_factors <- future_mapply(correct_mortality_factor, mor_data$geo, mor_data$sex, mor_data$age, MoreArgs = list(cause_of_death_data = dis_data))

mor_list <- mor_data %>% 
  group_by(geo, sex, age) %>% 
  nest() %>%
  mutate(cor_factor = pmap(list(geo, sex, age), ~dis_data %>% filter(geo == ..1, sex == ..2, over65 == ifelse(..3 < 65, 0, 1)) %>% pull(death_proportion)),
         cor_factor = map_dbl(cor_factor, ~ifelse(length(.x) > 0, .x, NA)),
         cor_data = map2(data, cor_factor, ~.x %>% mutate(values_uncorrected = values, values = values * (1-.y)))) %>%
  select(-data, -cor_factor) %>%
  unnest(cols = "cor_data")




saveRDS(mor_list, file = paste("data_input/generated/mortality_data.RDS", sep = ""))

### Harmonized index of consumer prices

hicp <- get_eurostat(id = "prc_hicp_midx") %>%
  filter(!(geo %in% c("EA", "EEA", "EU", "EU15", "EU25", "EU27_2020", "EU28", "EA19", "EA16", "EA17", "EA18", "EU27_2007", "CPC1", "EA11", "EA12", "EA13", "EA15"))) %>%
  mutate(year = year(time), 
         month = month(time),
         geo = str_to_lower(countrycode(geo, origin = "eurostat", destination = "iso2c"))) %>%
  filter(coicop == "CP00", unit == "I15", month == 6) %>%
  select(geo, year, values)

write.csv(hicp, file = "data_input/financial/cpi.csv")

### PPP

ppp_oecd <- get_dataset("SNA_TABLE4") %>%
  filter(TRANSACT == "PPPGDP",
         obsTime >= 2000,
         !(LOCATION %in% c("EA19", "EU28", "EU27_2020"))) %>%
  mutate(geo = str_to_lower(countrycode(LOCATION, origin = "iso3c", destination = "iso2c"))) %>%
  select(geo, unit = UNIT, year = obsTime, value = obsValue)

ppp_currencies <- ppp_oecd %>%
  arrange(year) %>%
  group_by(geo) %>%
  summarise(curr = last(unit))



ppp_euro <- get_eurostat(id = "prc_ppp_ind") %>%
  filter(na_item == "PPP_EU27_2020",
         ppp_cat == "GDP",
         time > "1999-12-31") %>%
  filter(!(geo %in% c("EU15", "EU25", "EU27_2020", "EU28", "EA19", "EA16", "EA17", "EA18", "EU27_2007", "CPC1", "EA11", "EA12", "EA13", "EA15"))) %>%
  mutate(geo = str_to_lower(countrycode(geo, origin = "eurostat", destination = "iso2c")),
         year = year(time)) %>%
  select(geo, year, value = values) %>%
  inner_join(ppp_currencies, by = "geo")


                    
write.csv(ppp_euro, file = "data_input/financial/ppp.csv")

### Exchange rates

exc_rate <- get_eurostat(id = "ert_bil_eur_m") %>%
  mutate(year = year(time),
         month = month(time)) %>%
  filter(month == 6,
         statinfo =="AVG") %>%
  select(currency, year, values)

write.csv(exc_rate, file = "data_input/financial/exchange_rates.csv")

### Productivity losses
#minimum wages as a proportion of average wages
minimum_wages <- get_eurostat(id = "earn_mw_avgr2") %>%
  mutate(year = year(time), 
         prop_min_wage = values/100) %>%
  filter(indic_se == "MMW_MEAN_ME_PP", 
         nace_r2 == "B-S",
         year %in% c(2016,2012,2008)) %>%
  select(geo, year, prop_min_wage) 

  

#average wages
# wages_data <- get_dataset("AV_AN_WAGE") 
# 
# wages_data2 <- wages_data %>%
#   filter(COUNTRY != "OECD",
#          SERIES == "CNPNCU") %>%
#   mutate(geo = str_to_lower(countrycode(COUNTRY, origin = "iso3c", destination = "iso2c")),
#          year = as.double(obsTime),
#          cur_year = as.double(REFERENCEPERIOD)) %>%
#   filter(geo %in% minimum_wages$geo) %>%
#   arrange(-year) %>%
#   group_by(geo) %>%
#   summarise(UNIT = first(UNIT),
#             year = first(year),
#             cur_year = first(cur_year),
#             value = first(obsValue)) 


#labour costs per employee in fte, per year
labour_costs <- get_eurostat(id = "lc_ncost_r2") %>%
  filter(currency == "NAC",
         nace_r2 == "B-S_X_O",
         lcstruct != "D1111",
         sizeclas == "GE10",
         unit == "P_SAL_Y") %>%
  mutate(year = year(time)) %>%
  pivot_wider(names_from = lcstruct, values_from = values) %>%
  select(geo, year, labour_cost_annual = D01, wages_salaries_annual = D111)

#average annual hours worked per employee
time_worked <- get_eurostat(id = "lc_nnum2_r2")%>%
  filter(indic_lc == "HP_SAL_AVG",
         nace_r2 == "B-S_X_O",
         sizeclas == "GE10",
         worktime == "AVG_FTE") %>% 
  mutate(year = year(time)) %>%
  select(geo, year, annual_hours_paid = values)

labour_export <- left_join(labour_costs, time_worked, by = c("geo", "year")) %>%
  left_join(minimum_wages, by = c("geo", "year")) %>%
  filter(!(geo %in% c("EU15", "EU25", "EU27_2020", "EU28", "EA19", "EA16", "EA17", "EA18", "EU27_2007"))) %>%
  mutate(labour_cost_perhour = labour_cost_annual / annual_hours_paid,
         wages_perhour = wages_salaries_annual / annual_hours_paid,
         av_hours_perday = annual_hours_paid / 365.25,
         labour_cost_perday = av_hours_perday * labour_cost_perhour,
         wages_perday = av_hours_perday * wages_perhour,
         wages_perhour_minimum = wages_perhour * prop_min_wage,
         geo = str_to_lower(countrycode(geo, origin = "eurostat", destination = "iso2c"))) %>%
  filter(year == 2016) %>%
  select(geo, year, labour_cost_daily = labour_cost_perday, labour_cost_hourly = labour_cost_perhour, wages_daily = wages_perday, wages_minimum_hourly = wages_perhour_minimum)
  

# wages_selection <- wages_data %>% filter(COUNTRY == countrycode_OECD,
#                                     SERIES == "CNPNCU") %>%
#   mutate(obsTime = as.numeric(obsTime)) %>%
#   as_tsibble(key = c(COUNTRY, SERIES),
#              index = obsTime) %>%
#   filter(obsTime == max(obsTime)) %>%
#   mutate(convValue = convert_price(obsValue, countrycode, year_out = price_level, year_in = obsTime)) %>%
#   select(currencyyear = REFERENCEPERIOD, annual_wages = convValue) %>%
#   mutate(daily_wages = (annual_wages / 52 / 5),
#          daily_minimum_wages = daily_wages * minimum_wages$prop)

saveRDS(labour_export, file = paste0("data_input/generated/wages.RDS"))

###unemployment, education and retirement

unemployment <- get_eurostat(id = "une_rt_a") %>%
  filter(!(geo %in% c("FX", "EU15", "EU25", "EU27_2020", "EU28", "EA19", "EA16", "EA17", "EA18", "EU27_2007")),
         sex == "T",
         time == "2019-01-01",
         unit == "PC_POP") %>%
  mutate(age = str_sub(age, start = 2),
         values = values/100,
         type = "unemployment",
         geo = str_to_lower(countrycode(geo, origin = "eurostat", destination = "iso2c"))) %>%
  select(type, geo, age, values) 

#education
education <- get_eurostat(id = "educ_uoe_enra05") %>%
  filter(!(geo %in% c("EU15", "EU25", "EU27_2020", "EU28", "EA19", "EA16", "EA17", "EA18", "EU27_2007")),
        time == "2018-01-01",
         unit == "PC",
         age == "Y15-24") %>%
  mutate(age = str_sub(age, start = 2),
         type = "education",
         values = values/100,
         geo = str_to_lower(countrycode(geo, origin = "eurostat", destination = "iso2c"))) %>%
  select(type, geo, age, values)



pension <- get_eurostat(id = "lfsq_ipga") %>%
  filter(!(geo %in% c("EU15", "EU25", "EU27_2020", "EU28", "EA19", "EA16", "EA17", "EA18", "EU27_2007"))) %>%
  filter(time == "2020-01-01",
         unit == "PC",
         sex == "T") %>%
  mutate(age = str_sub(age, start = 2),
         type = "pension",
         values = values/100,
         geo = str_to_lower(countrycode(geo, origin = "eurostat", destination = "iso2c"))) %>%
  filter(age %in% c("55-64", "65-69", "70-74")) %>%
  select(type, age, geo, values) 

#for Germany pension data are not available, so use Dutch data
pension_de <- pension %>% filter(geo == "nl") %>%
  mutate(geo = "de")

pension <- pension %>% add_row(pension_de)

bind_rows(unemployment, education, pension) %>%
  saveRDS(file = paste0("data_input/generated/employment.RDS"))



#influenza vaccination data
vac_inf <- get_eurostat(id = "hlth_ps_immu") %>%
  group_by(geo) %>%
  arrange(time) %>%
  summarise(value = last(values),
            reference_year = year(last(time))) %>%
  mutate(vac = "influenza",
         age = ">=65",
         value = value/100,
         geo = str_to_lower(countrycode(geo, origin = "eurostat", destination = "iso2c"))) %>%
  select(geo, vac, age, value, reference_year)

saveRDS(vac_inf, file = paste0("data_input/generated/vaccination_status.RDS"))


#average length of stay
#generated once for all countries
hos_los <- get_eurostat(id = "hlth_co_inpst") %>%
  arrange(time) %>%
  filter(sex %in% c("M", "F"),
         icd10 %in% c("J", "J00-J11", "J12-J18", "J20-J22"),
         age != "TOTAL") %>%
  group_by(geo, age, sex, icd10) %>%
  summarise(av_los = last(values),
            reference_year = year(last(time)),
            .groups = "drop") %>%
  mutate(hosp_reason = case_when(icd10 == "J" ~ "all rt",
                                 icd10 == "J00-J11" ~ "urti/inf",
                                 icd10 == "J12-J18" ~ "pneumonia",
                                 icd10 == "J20-J22" ~ "other lrti"),
         min_age = case_when(age == "Y_LT1" ~ 0,
                             age == "Y_GE95" ~ 95,
                             is.factor(age) ~ as.numeric(str_extract(age, "\\d+"))),
         max_age = case_when(age == "Y_LT1" ~ 0,
                             age == "Y_GE95" ~ 100,
                             is.factor(age) ~ as.numeric(str_extract(age, "(?<=-)\\d+"))),
         country = str_to_lower(geo),
         reference = "eurostat") %>%
  select(country, agerange = age, min_age, max_age, sex, av_los, hosp_reason, icd10, reference_year, reference) %>%
  arrange(min_age)

write.csv(hos_los, file = paste0("data_input/microsim_data/hospital_los.csv"), row.names = FALSE)

  
# hospitalizations by ICD code from eurostat
# https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=hlth_co_disch2&lang=en

hosp_number_import <- get_eurostat(id = "hlth_co_disch2")

hosp_number <- hosp_number_import %>%
  filter(icd10 %in% c(
    "J00-J11", #Acute upper respiratory infections and influenza
    "J12-J18", #Pneumonia
    "J20-J22", #Other acute lower respiratory infections
    "UPRESPIR_OTH", #Other diseases of upper respiratory tract (J30-J34, J36-J39)
    "J40-J44_J47" #Other lower respiratory diseases
  ), sex != "T", !(age %in% c("TOTAL", "Y_GE90")),
  time > "2009-01-01") %>%
  mutate(
    across(.cols= 2:6, .fns = as.factor),
    geo = str_to_lower(countrycode(geo, origin = "eurostat", destination = "iso2c")),
    age = case_when(age == "Y_GE95" ~ "Y95-120",
                    age == "Y_LT1" ~ "Y0-0",
                     is.character(age) ~ age),
    min_age = str_extract(age, "(?<=Y)[:digit:]+"),
    max_age = str_extract(age, "(?<=-)[:digit:]+")) %>%
  select(geo, icd10, sex, min_age, max_age, values)

write.csv(hosp_number, file = "data_input/generated/hospital_adm.csv", row.names = FALSE)
# Health expenditure ---------------------------------------------------------
# https://ec.europa.eu/eurostat/databrowser/view/hlth_sha11_hf/default/table?lang=en 
health_exp <- get_eurostat(id = "hlth_sha11_hf")
health_exp$icha11_hf <- as.factor(health_exp$icha11_hf)

#MIO_NAC = million units national currency
#NAC_HAB = national currency per inhabitant
#PC_CHE = Percentual share of current health expenditure
#MIO_PPS_EU27_2020 Million PPS from EU27 2020
#PC_GDP Percentage of GDP

exp <- health_exp %>%
  filter(!(geo %in% c("EU15", "EU25", "EU27_2020", "EU28", "EA19", "EA16", "EA17", "EA18", "EU27_2007", "EA12")),
         unit == "MIO_NAC" | unit == "NAC_HAB" | unit == "PC_CHE" | unit == "MIO_PPS_EU27_2020" | unit == "PC_GDP") %>%
  mutate(fin_scheme = as.factor(case_when(icha11_hf == "TOT_HF" ~ "All financing schemes",
                                icha11_hf == "HF1" ~ "Government schemes and compulsary contributory health care financing schemes",
                                icha11_hf == "HF2" ~ "Voluntary health care payment schemes",
                                icha11_hf == "HF3" ~ "Household out-of-pocket payment",
                                icha11_hf == "HF4" ~ "Rest of the world financing schemes",
                                icha11_hf == "HF_UNK" ~ "Financing schemes unknown")),
         geo = str_to_lower(countrycode(geo, origin = "eurostat", destination = "iso2c")),
         time = year(time))


exp_nl <- exp %>% filter(geo == "nl", unit == "MIO_PPS_EU27_2020", fin_scheme == "All financing schemes") %>% as_tsibble(index = "time")


fit <- exp_nl %>% model(ETS( values ~ error("A") + trend("A") + season("N")))

fc <- fit %>% forecast(h = 30)

fc %>% autoplot(exp_nl)
                                  