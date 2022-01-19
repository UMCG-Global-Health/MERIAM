# This file contains the functions to create the data and results tibbles for use in model.R
#standard inputs for testing
# n_nodes <- 1000
# country_list <- c("gb")
# strategy_list <- c("base", "crp")
# rollout <- c(NA, .25)
# start_year <- 2020
# n_years <- 5
# included_weeks <- 1:52
# currency_year <- 2019
# productivity_method <- "friction"
# utility_valueset <- "EUVAS"
# use_corrected_mortality <- TRUE
# probabilistic <- FALSE
# iterations <- 1

create_data <- function(n_nodes, 
                        country_list, 
                        strategy_list, 
                        rollout = c(1, .25, .25),
                        start_year, 
                        n_years,
                        included_weeks = 1:52,
                        currency_year = 2019,
                        productivity_method = "friction",
                        utility_valueset = "EUVAS",
                        use_corrected_mortality = TRUE,
                        probabilistic = FALSE,
                        iterations = 1) {
  ##### HELPER FUNCTION FOR BASE FUNCTIONALITY #####
  create_data_helper <- function(iter = 1){
    #### import population data for each included country ####
    population_country <- future({
      population_import <- bind_rows(readRDS("data_input/generated/pop_proj.RDS"),
                                     readRDS("data_input/generated/data_uk/pop_proj.RDS"))
      
      migration_import <- bind_rows(readRDS("data_input/generated/migration_data.RDS"),
                                    readRDS("data_input/generated/data_uk/migration_data.RDS") %>%
                                      mutate(geo = ifelse(geo == "uk", "gb", geo)))
      mortality_import <- bind_rows(readRDS("data_input/generated/mortality_data.RDS"),
                                    readRDS("data_input/generated/data_uk/mortality_data.RDS") %>%
                                      mutate(geo = ifelse(geo == "uk", "gb", geo)))
      
      
      tibble(country = country_list,
             incuded_weeks = list(included_weeks)) %>%
        mutate(pop_project = map(country, ~population_import  %>% filter(geo == .x)),
               pop_start_year = map(pop_project, ~.x %>% filter(time == start_year)),
               age_dist = map(pop_start_year, ~.x %>% 
                                mutate(age = str_sub(age, start = 2, end = 4),
                                       age = ifelse(age == "_LT", 0, age)) %>%
                                filter(str_to_upper(sex) == "T",
                                       str_to_upper(age) != "_GE",
                                       str_to_upper(age) != "OTA") %>%
                                select(sex, age, values) %>%
                                mutate(age = as.double(age),
                                       ageprop = values / sum(values)) %>%
                                arrange(age)),
               female_prob = map(pop_start_year, ~.x %>% 
                                 mutate(age = str_sub(age, start = 2, end = 4),
                                        age = ifelse(age == "_LT", 0, age)) %>%
                                 filter(str_to_upper(sex) == "T" | sex == "F",
                                        str_to_upper(age) != "_GE",
                                        str_to_upper(age) != "OTA") %>%
                                 select(sex, age, values) %>%
                                 mutate(age = as.double(age)) %>%
                                 arrange(sex) %>%
                                 group_by(age) %>%
                                 summarise(prop = first(values) / last(values),
                                           .groups = "drop")),
               fertility = map(pop_project, ~.x %>% 
                                 mutate(age = str_sub(age, start = 2, end = 4),
                                        age = ifelse(age == "_LT", 0, age)) %>%
                                 filter(str_to_upper(sex) == "T",
                                        str_to_upper(age) != "_GE",
                                        str_to_upper(age) != "OTA") %>%
                                 mutate(age = as.double(age)) %>%
                                 select(time, age, values) %>%
                                 arrange(age) %>%
                                 pivot_wider(names_from = "age", values_from = "values") %>%
                                 rowwise() %>%
                                 mutate(parent_pop = sum(c_across(17:47)),
                                        fertility = `0` / parent_pop) %>%
                                 select(time, fertility) %>%
                                 ungroup()),
               mortality = map(country, ~mortality_import %>% filter(geo == .x)),
               migration = map(country, ~migration_import %>% filter(geo == .x)),
               migration = map2(migration, pop_project, ~left_join(.x,
                                                                   .y %>% filter(sex == "T", str_to_upper(age) == "TOTAL") %>% select(time, total_pop = values),
                                                                   by = "time") %>%
                                  mutate(rate = values / total_pop) %>%
                                  select(-geo, -values, -total_pop)),
               employment = map(country, ~readRDS("data_input/generated/employment.RDS") %>% filter(geo == .x) %>% select(-geo)),
               vaccination = map(country, ~readRDS("data_input/generated/vaccination_status.RDS") %>% filter(geo == .x)),
               utility_data = map(country, ~readRDS("data_input/uti/base_utilities.RDS") %>% 
                                 filter(geo == .x, value_set == utility_valueset) %>%
                                 rowwise() %>%
                                 mutate(alpha = mean^2 * (1-mean) / se^2 - mean, 
                                        beta = alpha * (1-mean) / mean,
                                        sample = rbeta(1, alpha, beta)) %>%
                                 select(sex, minage, maxage, mean, sample)),
               utility_data = map(utility_data, 
                               ~tibble(sex_2 = rep(0:1, each = 100),
                                       age = rep(0:99, times = 2)) %>%
                                 rowwise() %>%
                                 mutate(uti = ifelse(age < 18, 1, #for children, set base utility to 1
                                                     .x %>% 
                                                       filter(sex == ifelse(sex_2 == 0, "male", "female"),
                                                              minage <= age,
                                                              maxage >= age) %>%
                                                       pull(ifelse(probabilistic == TRUE,sample, mean)))) %>% 
                                 rename(sex = sex_2)),
               amr_elas = list(PRSP = read_rds("data_input/amr/prsp_elas.RDS")),
               # eurostat incidence import
               # hospital_admissions = map(country,
               #                           function(x, probabilistic){
               #                             import <- read.csv("data_input/generated/hospital_adm.csv") %>%
               #                               filter(geo == x) %>%
               #                               select(-geo) %>%
               #                               group_by(icd10, sex, min_age, max_age)
               #                             
               #                             if(probabilistic) {
               #                               #if the output should be probabilistic
               #                               #use a poisson distribution to predict the annual
               #                               #value from data from the past 10 years
               #                               output <- import %>% 
               #                                 nest() %>%
               #                                 mutate(value = map_dbl(data,
               #                                                        ~rpois(1, .x$values))) %>%
               #                                 select(-data) %>%
               #                                 ungroup()
               #                                 
               #                             } else {
               #                               #if the output should be deterministic
               #                               #use the mean of the past 10 years
               #                               output <- import %>%
               #                                 summarise(value = round(mean(values)),
               #                                           .groups = "drop") %>%
               #                                 ungroup()
               #                             }
               #                             return(output)
               #                             
               # }, probabilistic),
               age_distribution = map(age_dist, agedist, n_nodes),
               sex_distribution = map2(female_prob, age_distribution, sexdist),
               empl_distribution = map2(age_distribution, employment, calc_employment, "array"),
               nodes = pmap(list(sex = sex_distribution, age = age_distribution, empl = empl_distribution), 
                            node_create,
                            n = n_nodes, active = 1),
               nodes = pmap(list(nodes = nodes, data_vaccination = vaccination),
                            run_influenza_vaccine),
               nodes = map2(nodes, utility_data, run_base_utility)) %>%
        select(-pop_project, -female_prob, -age_dist,  -age_distribution, -sex_distribution, -empl_distribution)
      
    }, seed = TRUE)
    
    #### predict incidence of ca-arti, using incidence models ####
    # incidence <- 
    #   readRDS("data_input/who/models.RDS") %>%
    #   filter(country %in% country_list) %>%
    #   mutate(season = as.double(str_sub(season, 1,4))+4,
    #          week = list(included_weeks),
    #          incidence = pmap(list(model, season, week), ~incidence_predict(..1, ..2, ..3, probabilistic = probabilistic))) %>%
    #   unnest(c("week", "incidence")) %>%
    #   select(country, season, week, type, incidence) %>%
    #   mutate(week = str_c(season, "-",week)) %>%
    #   pivot_wider(names_from = type, values_from = incidence) %>%
    #   ungroup() %>%
    #   select(country, week, ari, ili)
    #   
    
    #if country is gb, select incidence for gb-eng
    country_list_incidence <- map_chr(country_list,
                   ~if_else(.x == "gb", "gb-eng", .x))
    
    incidence <- expand_grid(geo = country_list_incidence,
                  season = 1:(n_years+1),
                  type = c("ari", "ili")) %>%
        mutate(season = str_c(start_year+season-2,"-",start_year+season-1),
               import = map2(geo, type,
                             ~readRDS("data_input/ecdc/incidence_models.RDS") %>%
                               mutate(season = as.character(season)) %>%
                               filter(geo == .x, type == .y)),
               n_seasons = map_dbl(import,
                                   ~length(levels(as.factor(.x$season))))) %>%
        filter(n_seasons > 0) %>%
        mutate(q = sample((1:9999)/10000, nrow(.)),
               q = ifelse(probabilistic == TRUE, q, .5)) %>%
        rowwise() %>%
        mutate(rand_season = sample(1:n_seasons, 1)) %>%
        ungroup() %>%
        mutate(inc_data = pmap(list(import, q, rand_season),
                               function(x, q, rand_season){
                                 select_season <- levels(as.factor(x$season))[rand_season]
                                 
                                 x <- x %>%
                                   filter(season == select_season) %>%
                                   rowwise() %>%
                                   mutate(incidence = qlnorm(q, meanlog = logmean, sdlog = logsd)) %>%
                                   ungroup() %>%
                                   select(age_group, week = weeknr, incidence)
                                 
                                 expand_grid(agecat = levels(as.factor(x$age_group)),
                                             week = c(31:52, 1:30)) %>%
                                   left_join(x, by = c("agecat" = "age_group", "week")) %>%
                                   mutate(incidence = coalesce(incidence, 0))
                               })) %>%
        select(geo, season, type, inc_data) %>%
        unnest(inc_data) %>%
        mutate(year = case_when(
          week >= 31 ~ as.double(str_sub(season, 1, 4)),
          week < 31 ~ as.double(str_sub(season, 6, 9))
        ),
        epi_week = str_c(year, "-", formatC(week, width = 2, flag = "0")),
        geo = if_else(geo == "gb-eng", "gb", geo)) %>%
        select(geo, type, year, week, epi_week, agecat, incidence)
    
    # import cap data
    # cap_agents <- read_yaml("data_input/cap/causitive_agents_cap.yaml")
    # 
    # cap_data <- list(incidence = read_excel("data_input/cap/incidence_cfr_nl.xlsx", col_types = c("text", "text", "text", "text", "numeric", "numeric", "text", "numeric", "numeric", "skip")) %>%
    #                    filter(disease == "cap" | disease == "ipd") %>%
    #                    rowwise() %>%
    #                    mutate(sd = as.double(sd),
    #                           value = ifelse(probabilistic == FALSE | is.na(sd), value,  rnorm(1,  mean = value, sd = sd))) %>%
    #                    left_join(read_excel("data_input/cap/incidence_cfr_nl.xlsx", col_types = c("text", "text", "text", "text", "numeric", "numeric", "text", "numeric", "numeric", "skip")) %>%
    #                                filter(disease == "risk") %>%
    #                                select(agecat, `risk-group`, weight = value),
    #                              by = c("agecat", "risk-group")) %>%
    #                    group_by(disease, item, setting, agecat, .add = FALSE) %>%
    #                    summarise(min_age = mean(min_age),
    #                              max_age = mean(max_age),
    #                              mean = weighted.mean(value, weight),
    #                              .groups = "drop"),
    #                  agents = c(sp = ifelse(probabilistic == TRUE, 
    #                                  rbeta(1, cap_agents$cap$`Staphylococcus aureus`$alpha, cap_agents$cap$`Staphylococcus aureus`$beta),
    #                                  cap_agents$cap$`Staphylococcus aureus`$mean)))
    
    #### Create sim_input object
    #### 
    
    sim_input <- create_sim_input(country_list = country_list,
                                  strategy_list = strategy_list,
                                  n_years = n_years,
                                  included_weeks = included_weeks,
                                  currency_year = currency_year,
                                  productivity_method = productivity_method,
                                  probabilistic = probabilistic)

    #create tibble which includes the rollout of the different strategies
    rollout_tbl_start <- tibble(year = start_year:(start_year + n_years - 1))
    rollout_tbl <- tibble(year = integer(),
                          strategy = character(),
                          rollout = double())
    
    for(i in 1:length(rollout)){
      rollout_tbl_strat <- rollout_tbl_start %>%
        mutate(.nyear = 1:n_years,
               strategy = strategy_list[i],
               rollout = ifelse(.nyear*rollout[i] <= 1, .nyear*rollout[i], 1)) %>%
        select(year, strategy, rollout)
      
      rollout_tbl <- add_row(rollout_tbl, rollout_tbl_strat)
    }
    
    rm(rollout_tbl_start, rollout_tbl_strat, i) 
    
    #### LOAD population_country (future) value ####
    population_country <- value(population_country)
      
    
    
    #### select the use of corrected or uncorrected mortality ####
    if(use_corrected_mortality == TRUE){
      ##if use_corrected_mortality == TRUE: remove uncorrected values to save memory
      population_country$mortality <- map(population_country$mortality, ~select(.x, -values_uncorrected))
    } else if(use_corrected_mortality == FALSE){
      ##if use_corrected_mortality == FALSE: change values in mortality data to use uncorrected data
      population_country$mortality <- map(population_country$mortality, ~select(.x, -values) %>% 
                                            rename(values = values_uncorrected))
    }

    population_country %<>%
      mutate(incidence = map(country, ~incidence %>%
                               filter(geo == .x)))
    
    
    #### copy population data for each included strategy ####
    # up till now, the data were imported per country, now copy the population parameters for each
    # diagnostic strategy
    population <- population_country %>%
      slice(rep(1:nrow(population_country), each = length(strategy_list))) %>%
      mutate(strategy = rep(strategy_list, times = length(country_list)),
             rollout = map(strategy,
                           ~rollout_tbl %>%
                             filter(strategy == .x) %>%
                             select(-strategy))) %>%
      relocate(country, strategy, nodes)
    
    #### Add AMR data
    population <- population %>%
      mutate(
        amr = map2(country, iter,
                  ~read_csv("data_input/amr/data_projections/meriam_amr_forecasts.csv") %>%
                    filter(geo == str_to_upper(.x)) %>%
                    select(geo, pair, year, value = iter+4) %>%
                    group_by(geo, pair) %>%
                    nest() %>%
                    mutate(
                      abx = case_when(
                        str_sub(pair, 1, 3) == "3GC" ~ "3GC",
                        str_sub(pair, 1, 2) == "CR" ~ "C",
                        pair == "FREC" ~ "F",
                        pair == "MRSA" ~ "NSP",
                        pair == "PRSP" ~ "BSP",
                        pair == "VRE" ~ "G",
                        is.character(pair) ~ "other"
                      )
                    )),
        abx_con = map2(country, iter,
                      ~read_csv("data_input/amr/data_projections/meriam_cons_forecasts.csv", col_types = NULL) %>%
                        filter(geo == str_to_upper(.x)) %>%
                        select(code, year, mean = iter+5) %>%
                        distinct())
      )
    
    
    
    #### COMBINE POPULATION AND MICROSIM INPUT
    output <- left_join(population, sim_input, by = c("country", "strategy"))
  }
  
  if(length(iterations) == 1){
    create_data_helper(iter = iterations) %>%
      mutate(iteration = iterations) %>%
      relocate(country, strategy, iteration)
  } else if (length(iterations) > 1){
    #in case of a probabilistic model, where all model replications are nested
    #run create_data_helper() probabilistic for all iterations (in parallel if possible)
    create_data_probabilistic <- 
      foreach(i = iterations,
              .inorder = FALSE, 
              .combine = "rbind") %dorng% 
        {
          create_data_helper(iter = i) %>%
            mutate(iteration = i) %>%
            relocate(country, strategy, iteration)
        }
    create_data_probabilistic
    } 
  
  }
  
create_sim_input <- function(country_list, 
                             strategy_list, 
                             n_years,
                             included_weeks = 1:52,
                             currency_year = 2019,
                             productivity_method = "friction",
                             probabilistic = FALSE){
  #### LOAD MICROSIM INPUT ####
  sim_input <- 
    tibble(country = rep(country_list, each = length(strategy_list)),
           strategy = rep(strategy_list, times = length(country_list)),
           microsim_duration = 28,
           eurostat_financialdata = list(list( cpi = read.csv("data_input/financial/cpi.csv"),
                                          ppp = read.csv("data_input/financial/ppp.csv"),
                                          exchange_rate = read.csv("data_input/financial/exchange_rates.csv"))),
           utilities = list(read_yaml("data_input/microsim_data/microsim_utilities.yaml")),
           longterm = list(read_yaml("data_input/effects_longterm.yaml")),
           labour = map(country, ~readRDS(paste0("data_input/generated/wages.RDS"))),
           consult_costdata = list(read_csv("data_input/microsim_data/costs_consults.csv", 
                                            col_types = cols(ref = col_skip()))),
           antibiotic_types = list(read.csv("data_input/microsim_data/ppas/abx_type.csv")),
           tests = map(country, ~as_tibble(read.csv("data_input/microsim_data/ppas/tests.csv")))) %>%
    mutate(antibiotic_prescribing = list(read.csv("data_input/microsim_data/ppas/abx_prescriptions.csv")), 
           probabilities = list(microsim_load_params("data_input/microsim_data/microsim_probabilities.yaml", "base")),
           antibiotic_prescribing = map2(antibiotic_prescribing, country, ~filter(.x, country == .y)),
           antibiotic_types = map2(antibiotic_types, country, ~filter(.x, country == .y)),
           tests = map2(tests, country, ~filter(.x, country == .y)),
           consult_costdata = map2(consult_costdata, country, ~filter(.x, country == .y)),
           currency = if_else(country == "gb", "GBP", "EUR"))
  
  ## convert costs to currency year and apply stochasticity if probabilistic == TRUE ##
  sim_input$tests <- future_pmap(list(sim_input$tests, 
                                      sim_input$country,
                                      sim_input$eurostat_financialdata,
                                      sim_input$currency),
                                 ~..1 %>% mutate(cost = convert_price(cost, 
                                                                     country = ..2, 
                                                                     year_in = year,
                                                                     year_out = currency_year, 
                                                                     input = c("local", ..4),
                                                                     output = c("ppp", "EUR"),
                                                                     eurostat_data = ..3),
                                                probabilistic = probabilistic,
                                                p = ifelse(probabilistic, rbeta(n(), alpha, beta), p)) %>%
                                   select(test, p, cost) %>%
                                   drop_na(),
                                 .options = furrr_options(seed = TRUE))
  
  
  sim_input$antibiotic_types <- pmap(list(sim_input$antibiotic_types, 
                                          sim_input$country,
                                          sim_input$eurostat_financialdata,
                                          sim_input$currency),
                                     ~..1 %>% 
                                       mutate(cost = convert_price(cost, 
                                                                   country = ..2, 
                                                                   year_in = year,
                                                                   year_out = currency_year, 
                                                                   input = c("local", ..4),
                                                                   output = c("ppp", "EUR"),
                                                                   eurostat_data = ..3),
                                              probabilistic = probabilistic,
                                              p = ifelse(probabilistic, rbeta(n(), alpha, beta), p)) %>%
                                       select(code, abx_code, abx, p, n_days, delay, cost))
  
  
  sim_input$antibiotic_prescribing <- map(sim_input$antibiotic_prescribing,
                                          ~.x %>%
                                            mutate(probabilistic = probabilistic,
                                                   p = ifelse(probabilistic, rbeta(n(), alpha, beta), p)) %>%
                                            select(-probabilistic, -X, -reference, -country, -alpha, -beta))
  
  sim_input$consult_costdata <- pmap(list(sim_input$consult_costdata, 
                                          sim_input$country, 
                                          sim_input$eurostat_financialdata,
                                          sim_input$currency),
                                     ~..1 %>% mutate(cost = convert_price(cost, 
                                                                         country = ..2, 
                                                                         year_in = curyear,
                                                                         year_out = currency_year, 
                                                                         input = c("local", ..4),
                                                                         output = c("ppp", "EUR"),
                                                                         eurostat_data = ..3)) %>%
                                       select(-curyear))
  
  sim_input$labour <- pmap(list(sim_input$labour, 
                                sim_input$country, 
                                sim_input$eurostat_financialdata,
                                sim_input$currency),
                           ~..1 %>%
                             filter(geo == ..2) %>%
                             mutate(labour_cost_daily = convert_price(labour_cost_daily, 
                                                                      country = ..2, 
                                                                      year_in = year,
                                                                      year_out = currency_year, 
                                                                      input = c("local", ..4),
                                                                      output = c("ppp", "EUR"),
                                                                      eurostat_data = ..3),
                                    wages_daily = convert_price(wages_daily, 
                                                                country = ..2, 
                                                                year_in = year,
                                                                year_out = currency_year, 
                                                                input = c("local", ..4),
                                                                output = c("ppp", "EUR"),
                                                                eurostat_data = ..3),
                                    wages_minimum_hourly = convert_price(wages_minimum_hourly, 
                                                                         country = ..2, 
                                                                         year_in = year,
                                                                         year_out = currency_year, 
                                                                         input = c("local", ..4),
                                                                         output = c("ppp", "EUR"),
                                                                         eurostat_data = ..3),
                                    friction_period = 12,
                                    minimum_daily_nhours_unemployed = 1, #assume that all unpaid work for  is one hour per day on average
                                    minimum_daily_nhours_retired = 1, #assume that all unpaid work for  is one hour per day on average
                                    labour_loss_parent_factor = .5, #assume parents work for 50% with a sick child 
                                    ret_age = 65) %>%  
                             select(labour_cost_daily, 
                                    wages_daily, 
                                    wages_minimum_hourly, 
                                    friction_period, 
                                    minimum_daily_nhours_unemployed, 
                                    minimum_daily_nhours_retired, 
                                    labour_loss_parent_factor, 
                                    ret_age) %>%
                             as.list())
  
  # add method used to calculate productivity to sim_input$labour
  sim_input$labour <- map2(sim_input$labour, productivity_method,
                           ~append(.x, list(productivity_method = .y)))
  
  #long-term effects are imported as a list, change the deterministic values to 
  #a probabilistic value if needed
  if(probabilistic){
    for(i in 1:length(sim_input$longterm)){
      sim_input$longterm[[i]]$excess_mortality$cap$relative_risk <- rlnorm(1, log(sim_input$longterm[[i]]$excess_mortality$cap$relative_risk), (sim_input$longterm[[i]]$excess_mortality$cap$relative_risk_sd))
    }
  }
  
  #convert utilities in microsim to appropriate list and make probabilistic if applicable
  for(i in 1:length(sim_input$utilities)){
    q_names <- c("q_sick", "q_hospital", "q_icu", "q_healthy", "q_death")
    q_list <- rep(NA, length(q_names))
    for(j in 1:length(sim_input$utilities[[i]]$base_utilities)){
      if(q_names[j] == "q_death"){
        q_list[j] <- 0
      } else if(is.double(sim_input$utilities[[i]]$base_utilities[[j]]$value)){
        if(probabilistic){
          mean <- sim_input$utilities[[i]]$base_utilities[[j]]$value
          se <- sim_input$utilities[[i]]$base_utilities[[j]]$sd
          alpha <- mean^2 * (1-mean) / se^2 - mean
          beta <- alpha * (1-mean) / mean
          q_list[j] <- rbeta(1, alpha, beta)
        } else {
          q_list[j] <- sim_input$utilities[[i]]$base_utilities[[j]]$value
        }
      }
    }
    names(q_list) <- q_names
    sim_input$utilities[[i]]$base_utilities <- q_list
  }
  
  
  
  
  #### create matrix to store hospital LOS data for transition probabilities ####
  
  # data wrangling
  for(i in 1:length(sim_input$probabilities)){
    p_hc_data_input <- sim_input$probabilities[[i]]$p_hc$data %>%
      mutate(sex = ifelse(sex == "M", 0, 1),
             hosp_reason = case_when(hosp_reason == "urti/inf" ~ "ili",
                                     hosp_reason == "pneumonia" ~ "ari")) %>%
      filter(country == sim_input$country[[i]],
             hosp_reason %in% c("ili", "ari")) %>%
      select(hosp_reason, sex, min_age, max_age, av_los)
    
    #run this for both ari and ili and for all ages and sexes
    p_hc_data_temp_ari <- foreach(s = rep(0:1, each = 100), 
                                  a = rep(0:99, times = 2),
                                  .inorder = TRUE, .combine = "c") %do%
      {
        p_hc_data_input %>%
          filter(min_age <= a,
                 max_age >= a,
                 sex == s,
                 hosp_reason == "ari")  %>%
          pull(av_los)
      }
    
    p_hc_data_temp_ili <- foreach(s = rep(0:1, each = 100), 
                                  a = rep(0:99, times = 2),
                                  .inorder = TRUE, .combine = "c") %do%
      {
        p_hc_data_input %>%
          filter(min_age <= a,
                 max_age >= a,
                 sex == s,
                 hosp_reason == "ili")  %>%
          pull(av_los)
      }
    
    
    #create matrix
    p_hc_data_matrix <- matrix(data = c(p_hc_data_temp_ari, p_hc_data_temp_ili),
                               nrow = 200, ncol = 2,
                               byrow = FALSE)
    colnames(p_hc_data_matrix) <- c("ari", "ili")
    
    #assume a linear trend of hospitalization duration
    #where 50% is discharged at t = av_los (average lenght of stay)
    p_hc_data_matrix_slope <- .5/p_hc_data_matrix
    
    #making the cumulative hazard function: H(t) = slope * t
    #the transition probability is given as p
    #reference Briggs pages 52-53 for formulas to calculate transition probabilities
    
    sim_input$probabilities[[i]]$p_hc$data  <- 1-exp(-p_hc_data_matrix_slope)
    
    
    
  }
  return(sim_input)
}


apply_effects_systrev <- function(data, probabilistic = FALSE, currency_year = 2019){
  # load effects from systematic reviews
  effects <- read_yaml("data_input/microsim_data/effects_systrev.yaml")
  
  #change antibiotic prescribing rates based on data in effects
  data %<>% mutate(
    antibiotic_prescribing = map2(antibiotic_prescribing, strategy,
                                  function(antibiotic_prescribing, strategy, effects, probabilistic){
                                    effects <- effects %>%
                                      pluck(strategy) %>%
                                      pluck("abx_prescribing")
                                    
                                    if(probabilistic == FALSE){
                                      rr <- effects$value
                                    }else{
                                      # if probabilistic = TRUE, check the distribution to use
                                      if(effects$distribution == "none"){
                                        rr <- effects$value
                                      } else if(effects$distribution == "lognormal"){
                                        logmean <- log(effects$value)
                                        logsd <- (log(effects$upr) - log(effects$lwr)) / (2 * 1.96) 
                                        rr <- rlnorm(1, logmean, logsd)
                                      }else{
                                        return(warning("Warning in apply_effects_sysrev: unsupported distribution"))
                                      }
                                    }
                                    
                                    antibiotic_prescribing %>%
                                      mutate(p = case_when(
                                        par == "prescriptions" ~ p * rr,
                                        is.character(par) ~ p
                                      ))
                                  }, effects, probabilistic),
    tests = pmap(list(tests, strategy, country, currency),
                 function(tests, strategy, country, currency, effects, currency_year){
                   test_data <- effects %>%
                     pluck(strategy) %>%
                     pluck("test") %>%
                     as_tibble %>%
                     rename(test = name) %>%
                     mutate(cost = convert_price(cost, 
                                                 country, 
                                                 year, 
                                                 currency_year, 
                                                 input = c("local", currency),
                                                 output = c("ppp", "EUR"))) %>%
                     select(-year)
                   
                   tests %>% 
                     add_row(test_data)
                     
                 }, effects, currency_year))
    
    return(data)
}

#create table to store results
create_resultstable <- function(data,
                                discount_rate = c(health = .03, economic = .03)){
  results <- data %>%
    select(country, strategy, iteration) %>%
    mutate(discount_health = discount_rate["health"],
          discount_economic = discount_rate["economic"],
          mortality = list(tibble(year = double(),
                                  base = double(),
                                  excess = double(),
                                  overlap = double())),
          pyramid = list(tibble(year = double(),
                                popGroup = character(),
                                sex = character(),
                                n = double())),
          ly = list(tibble(year = double(),
                           week = double(),
                           lifeyear = integer(),
                           qaly = integer())),
          microsim_aggregated = list(tibble( year = numeric(),
                                             week = numeric(),
                                             incidence = numeric(),
                                             costs = numeric(),
                                             lifeyear = numeric(),
                                             qaly = numeric(),
                                             deaths = numeric(),
                                             hosp = numeric(),
                                             prod = numeric(),
                                             runtime = duration())),
          microsim_hospitalized = list(tibble(year = numeric(),
                                              week = numeric(),
                                              id = numeric(),
                                              age = numeric(),
                                              sex = numeric())),
          microsim_abxcon = list(tibble(year = numeric(),
                                        week = numeric(),
                                        abx = character(),
                                        ddd = numeric()
          )),
          hc_product = list(tibble( year = numeric(),
                                    week = numeric(),
                                    product = numeric())),
          amr_rates = list(tibble(year = double(),
                                  amr = list())))
  
  return(results)
}
