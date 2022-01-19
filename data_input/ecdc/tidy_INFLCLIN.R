checkpoint::checkpoint("2021-03-01", checkpointLocation = getwd(), scanForPackages = FALSE)

library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(countrycode)
library(ggplot2)
library(incidence)
library(aweek)
library(timetk)


# import TESSY data from ECDC, not publicly available, but can be requested if needed
# more info: https://www.ecdc.europa.eu/en/publications-data/request-tessy-data-research

data_import <- read_csv("data_input/ecdc/INFLCLIN_Haggregated_2005-2020.csv", 
                                           col_types = cols(`ARI_Denominator00-04` = col_double(), 
                                                            `ARI_Denominator05-14` = col_double(), 
                                                            `ARI_Denominator15-64` = col_double(), 
                                                            `ARI_Denominator65+` = col_double(), 
                                                            ARI_DenominatorNumberOfCases = col_double(), 
                                                            ARI_DenominatorUnk = col_double(), 
                                                            `ILI_Denominator00-04` = col_double(), 
                                                            `ILI_Denominator05-14` = col_double(), 
                                                            `ILI_Denominator65+` = col_double(), 
                                                            ILI_DenominatorNumberOfCases = col_double(), 
                                                            ILI_DenominatorUnk = col_double()))


#make data tidy
data_edit <- data_import %>%
  mutate(geo = if_else(ReportingCountry == "XK", "xk", str_to_lower(countrycode(ReportingCountry, origin = "eurostat", dest = "iso2c"))),
         geo = case_when(DataSource == "UK-NI_INFLUENZA" ~ "gb-nir",
                         DataSource == "UK-WALES_INFLUENZA" ~ "gb-wls",
                         DataSource == "UK-ENG_INFLUENZA" ~ "gb-eng",
                         DataSource == "UK-SCOT_INFLUENZA" ~ "gb-sct",
                         is.character(geo) ~ geo),
         geo = as.factor(geo),
         year = as.double(str_sub(DateUsedForStatisticsIso, 1,4)),
         week = as.double(str_sub(DateUsedForStatisticsIso, 7,8))) %>%
  rowwise() %>%
  mutate(`ILI_Denominator15-64` =    ILI_DenominatorNumberOfCases - sum(`ILI_Denominator00-04`,`ILI_Denominator05-14`,`ILI_Denominator65+`,ILI_DenominatorUnk, na.rm = TRUE),
          `inc_ARI00-04` = (`ARI00-04` / `ARI_Denominator00-04`)*100000,
          `inc_ARI05-14` = (`ARI05-14` /  `ARI_Denominator05-14`)*100000,
          `inc_ARI15-64` =  (`ARI15-64` / `ARI_Denominator15-64`)*100000,
          `inc_ARI65+` = (`ARI65+` / `ARI_Denominator65+`)*100000,
         `inc_ILI00-04` = (`ILI00-04` / `ILI_Denominator00-04`)*100000,
         `inc_ILI05-14` = (`ILI05-14` / `ILI_Denominator05-14`)*100000,
         `inc_ILI15-64` = (`ILI15-64` / `ILI_Denominator15-64`)*100000,
         `inc_ILI65+` = (`ILI65+` / `ILI_Denominator65+`)*100000,
         season = case_when(
          week <= 30 ~ str_c(year-1, "-",year),
          week > 30 ~ str_c(year, "-", year + 1)
         ),
         season = as.factor(season))


data_tidy <- data_edit %>%
  ungroup() %>%
  select(geo,
         36:46) %>%
  pivot_longer(cols = contains("inc"), names_to = "type", names_prefix = "inc_", values_to = "incidence") %>%
  mutate(age_cat = as.factor(str_sub(type, 4)),
          type = as.factor(str_to_lower(str_sub(type, 1, 3))),
         iso_week = get_aweek(week = week, year = year)) %>%
  arrange(week)

#start correcting for varying data availabity in the different countries
#filter out years for which less than 10 weeks are available
data_nest <- data_tidy %>%
  group_by(geo, type, season) %>%
  filter(geo %in%
           c(
             "nl",
             "be",
             "de",
             #"gb-nir",
             #"gb-wls",
             "gb-eng",
             #"gb-sct",
             "it",
             "fr",
             "es"
           ),
         season %in% c("2015-2016" ,"2016-2017", "2017-2018", "2018-2019"),
         week != 53) %>%
  mutate(date = week2date(iso_week)) %>%
  arrange(date) %>%
  remove_missing(na.rm = TRUE) %>%
  nest() %>%
  ungroup() %>%
  arrange(geo) %>%
  mutate(start = map_dbl(data,
                         ~first(.x$week)),
         end = map_dbl(data,
                       ~last(.x$week)),
         nweeks = map_dbl(data,
                          ~nrow(.x)/4),
         #exp_weeks = (52-start) + end + 1,
         data_imp = map(data,
                        ~.x %>% pivot_wider(names_from = age_cat,
                                            names_prefix = "age_cat_",
                                            values_from = incidence) %>%
                          pad_by_time(.date_var = "date", .by = "week") %>%
                          mutate(iso_week = date2week(date),
                                 year = as.double(str_sub(iso_week, 1, 4)),
                                 week = as.double(str_sub(iso_week, 7, 8)),
                                 across(starts_with("age_cat_"),
                                        ~ts_impute_vec(.x, period = 1, lambda = NULL))) %>%
                          pivot_longer(cols = starts_with("age_cat_"), names_to = "age_cat", names_prefix = "age_cat_", values_to = "incidence"))) %>%
  filter(nweeks > 10) 
  

#create incidence object and incidence models
data_incidence <- data_nest %>%
  mutate(data_imp = map2(data_imp, type,
                         function(data, type){
                           if(type == "ari"){
                             return(data)
                           }else{
                             data_new <- data %>%
                               filter(week >= 40 | week <= 20)
                             return(data_new)
                           }
                         })) %>%
  mutate(incidence_object = map(data_imp,
                                function(x){
                                    inc_obj <- incidence(dates = x$date, interval = "ISOweek", groups = x$age_cat)
                                    inc_obj$counts <- x %>%
                                      pivot_wider(values_from = incidence, names_from = age_cat) %>%
                                      select(`00-04`, `05-14`,  `15-64`, `65+`) %>%
                                      as.matrix()
                                    return(inc_obj) }),
         incidence_fit = map(incidence_object,
                             ~fit_optim_split(.x, quiet = TRUE)),
         incidence_models = map(incidence_fit,
                                ~.x$fit))


#create newdata frames to be able to predict
#ari is predicted in weeks 31 - 30
# ili is predicted in weeks 40 - 20
data_newdata <- data_incidence %>%
  mutate(split_date = map(incidence_fit,
                           ~.x$split),
         `age_00-04` = map_dbl(incidence_fit,
                           ~.x$split["00-04"] %>%
                             date2week() %>%
                             str_sub(7, 8) %>%
                             as.double()),
         `age_05-14` = map_dbl(incidence_fit,
                               ~.x$split["05-14"] %>%
                                 date2week() %>%
                                 str_sub(7, 8) %>%
                                 as.double()),
         `age_15-64` = map_dbl(incidence_fit,
                               ~.x$split["15-64"] %>%
                                 date2week() %>%
                                 str_sub(7, 8) %>%
                                 as.double()),
         `age_65+` = map_dbl(incidence_fit,
                               ~.x$split["65+"] %>%
                                 date2week() %>%
                                 str_sub(7, 8) %>%
                                 as.double())) %>%
  pivot_longer(cols = starts_with("age_"), names_to = "age_group", names_prefix = "age_", values_to = "split_week") %>%
  mutate(
    new_data = pmap(list(start, data, season, split_date, age_group, type),
                    function(start, data, season, split_date, age_group, type){
                      split_week <- split_date[age_group] %>%
                        date2week() %>%
                        str_sub(7, 8) %>%
                        as.double()
                      
                      if(type == "ari"){
                        #create v.weeks, where week 31 == v.week 0 and v.week 30 == week 51
                        
                        v.split_week <- ifelse(split_week >= 31, split_week - 31, split_week + 21)
                        
                        v.start <- ifelse(start >= 31, start - 31, start + 21)
                        
                        ndata <- data %>%
                          filter(age_cat == age_group) %>%
                          select(week, date) %>%
                          pad_by_time(.date_var = "date",
                                      .by = "week",
                                      .start_date = week2date(str_c(str_sub(season, 1, 4),"-W31-1")),
                                      .end_date = week2date(str_c(str_sub(season, 6, 9),"-W30-1"))) %>%
                          mutate(week = date2week(date),
                                 weeknr = as.double(str_sub(week, 7,8)),
                                 v.weeknr = ifelse(weeknr >= 31, weeknr - 31, weeknr + 21)) %>%
                          filter(weeknr < 53) %>%
                          mutate(dates.x.before = (v.weeknr - v.start)*7+3.5,
                                 dates.x.after = (v.weeknr - v.split_week)*7+3.5,
                                 model.select = as.factor(ifelse(v.weeknr < v.split_week, "before", "after")),
                                 dates.x = case_when(
                                   model.select == "before" ~ dates.x.before,
                                   model.select == "after" ~ dates.x.after
                                 ))
                      } else if(type == "ili"){
                        #create v.weeks, where week 40 == v.week 0 and v.week 20 == week 31
                        
                        v.split_week <- ifelse(split_week >= 40, split_week - 40, split_week + 12)
                        
                        v.start <- ifelse(start <= 40, 0, start - 40)
                        
                        ndata <- data %>%
                          filter(age_cat == age_group,
                                 week >= 40 | week <= 20) %>%
                          select(week, date) %>%
                          pad_by_time(.date_var = "date",
                                      .by = "week",
                                      .start_date = week2date(str_c(str_sub(season, 1, 4),"-W40-1")),
                                      .end_date = week2date(str_c(str_sub(season, 6, 9),"-W20-1"))) %>%
                          mutate(week = date2week(date),
                                 weeknr = as.double(str_sub(week, 7,8)),
                                 v.weeknr = ifelse(weeknr >= 40, weeknr - 40, weeknr + 12)) %>%
                          filter(weeknr < 53) %>%
                          mutate(dates.x.before = (v.weeknr - v.start)*7+3.5,
                                 dates.x.after = (v.weeknr - v.split_week)*7+3.5,
                                 model.select = as.factor(ifelse(v.weeknr < v.split_week, "before", "after")),
                                 dates.x = case_when(
                                   model.select == "before" ~ dates.x.before,
                                   model.select == "after" ~ dates.x.after
                                 ))
                      }
                      
                      
                      return(ndata)
                     
                    }),
    before_model = map2(age_group, incidence_models,
                        ~.y[[.x]]$before$model),
    after_model = map2(age_group, incidence_models,
                       ~.y[[.x]]$after$model)) %>%
  select(geo, season, age_group, type, data, start, end, split_date, incidence_object, new_data, before_model, after_model)

#do the actual predicting
data_predict <- data_newdata %>%
  mutate(new_data.before = map(new_data,
                               ~.x %>%
                                 filter(model.select == "before") %>%
                                 pull("dates.x")),
         new_data.after = map(new_data,
                              ~.x %>%
                                filter(model.select == "after") %>%
                                pull(dates.x)),
         predict.before = map2(before_model, new_data.before,
                              ~as_tibble(exp(predict(.x, newdata = data.frame(dates.x = .y), interval = "confidence")))),
         predict.after = map2(after_model, new_data.after,
                              ~as_tibble(exp(predict(.x, newdata = data.frame(dates.x = .y), interval = "confidence")))),
         predict = map2(predict.before, predict.after,
                         bind_rows),
         new_data = map2(new_data, predict,
                         ~tibble::add_column(.x, .y) %>%
                           select(weeknr, fit, lwr, upr))) %>%
  select(-before_model, -after_model)

#check the error and visual differences 
data_check <- data_predict %>%
  select(geo, season, age_group, type, new_data) %>%
  unnest(new_data) %>%
  group_by(geo, season, type) %>%
  nest() %>%
  mutate(check_plot = map(data,
                          ~.x %>% mutate(weeknr = factor(weeknr, levels = c(31:52, 1:30), ordered = TRUE)) %>%
                            ggplot(aes(x = weeknr, y = fit, fill = age_group)) +
                            geom_col()),
         inc_object = pmap(list(geo, season, type),
                           ~data_incidence %>%
                             filter(geo == ..1,
                                    season == ..2,
                                    type == ..3) %>%
                             pull("incidence_object") %>%
                             pluck(1)),
         orig_plot = map(inc_object,
                         ~plot(.x)),
         new_totalcases = map_dbl(data,
                                  ~sum(.x %>% pull("fit"))),
         orig_totalcases = map_dbl(inc_object,
                                   ~sum(.x$counts)),
         error = abs(new_totalcases - orig_totalcases) / ((new_totalcases + orig_totalcases) / 2))
         
data_output <- data_predict %>%
  select(geo, season, age_group, type, new_data) %>%
  unnest(new_data) %>%
  rowwise() %>%
  mutate(logmean = log(fit),
          logsd = (log(upr) - log(lwr)) / (2*1.96)
         ) %>%
  ungroup() %>%
  select(geo, season, age_group, type, weeknr, logmean, logsd)
        

saveRDS(data_output, "data_input/ecdc/incidence_models.RDS")


data <- data_output %>%
  filter(geo == "be", type == "ili", season == "2015-2016")

incidence_predict <- function(data, probabilistic = FALSE){
  q <- ifelse(probabilistic, sample((1:9999)/10000, 1), .5)
  
  
  data %>%
    rowwise() %>%
    mutate(logsd = (log(upr) - log(lwr)) / (2*1.96),
           logmean = log(fit),
           incidence = exp(qnorm(q, mean = logmean, sd = logsd)))
}
  


