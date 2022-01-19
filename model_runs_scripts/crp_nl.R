
# Setup -------------------------------------------------------------------

# clean the workspace
rm(list=ls())

# set parameters
params <- list(
  n_nodes = 100000,
  start_year = 2020,
  n_years = 10,
  save_dir = "/data/p268037/meriam/7dec"
)

print(params)

n_iter <- 2000
n_cores <- 10

print(n_iter)

## Load packages -----------------------------------------------------------

# load checkpoint of CRAN - used to improve portability and compatibility and packages
checkpoint::checkpoint("2021-07-20", checkpoint_location = getwd())
#checkpoint::checkpoint("2021-07-20", checkpointLocation = getwd())

library(tibble)
library(yaml)
library(compiler)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(furrr)
library(forcats)
library(stringr)
library(readxl)
library(readr)
library(lubridate)
library(doFuture)
#library(future.apply)
library(doRNG)
library(incidence)
library(truncnorm)
library(ggplot2)
library(AMR)

# compile functions using bytecompile for faster execution
enableJIT(3)

# source scripts with functions used in the model
source("functions/stats.R") #load population functions
source("functions/pop.R") #load population functions
source("functions/consults.R") #load consult functions
source("functions/ppas.R") #load antibiotic prescription functions
source("functions/productivity.R") #load productivity functions
source("functions/model_data-import.R") #load functions to load data
source("functions/microsim.R") #load microsim functions
source("functions/amr.R") #load amr functions
run_crp_comparison <- function(iter, params){
  # Input and load data -----------------------------------------------------
  
  start_year <- params$start_year
  week_interval <- 13
  n_years <- params$n_years
  iterations <- iter
  set_probabilistic = TRUE
  data <- create_data(n_nodes = params$n_nodes,
                      country_list = c("nl"),
                      strategy_list = c("base", "conservative" , "uncertain"),
                      rollout = c(1, .334, .334),
                      start_year = start_year,
                      n_years = n_years,
                      included_weeks = 1:52,
                      currency_year = 2019,
                      productivity_method = "friction",
                      use_corrected_mortality = FALSE,
                      probabilistic = set_probabilistic,
                      iterations = iterations) %>%
    apply_effects_systrev(probabilistic = set_probabilistic, currency_year = 2019)
  
  results <- create_resultstable(data,
                                 discount_rate = c(health = .015, 
                                                   economic = .04))
  
  save_dir <- params$save_dir
  
  
  # Run model ---------------------------------------------------------------
  
  current_year <- start_year
  
  ## get and store starting value of AMR ##
  current_amr <- calc_amr(data, results, current_year, initialize = TRUE)
  results %<>% mutate(
    amr_rates = pmap(list(country, strategy, iteration, amr_rates),
                     ~add_row(..4, current_amr %>%
                                filter(country == ..1,
                                       strategy == ..2,
                                       iteration == ..3) %>%
                                select(year, amr))))
  
  #record pyramid at the start
  results$pyramid <- map2(results$pyramid, data$nodes, create_pyramid, start_year)
  
  p <- Sys.time()
  
  for(i in 1:n_years){
    #create runsim object or empty it if already created in a previous year
    runsim <- list()
    for(w in 1:ceiling(length(data$incuded_weeks[[1]]) / week_interval)){
      #generate the weeknumbers that are calculated
      weeks <- data$incuded_weeks[[1]][((w-1)*week_interval+1):(w*week_interval)]
      
      
      ## Index consultation ------------------------------------------------------
      
      data %<>% consult_gp(weeks, current_year)
      
      
      ## Results ---------------------------------------------------------
      
      results %<>%
        mutate(microsim_aggregated = future_map2(microsim_aggregated, data$careseek,
                                                 function(dat, careseek, current_year, current_week){
                                                   careseek_aggr <- careseek %>%
                                                     summarise(
                                                       year = current_year,
                                                       week = current_week,
                                                       incidence = n(),
                                                       costs = NA,
                                                       lifeyear = NA,
                                                       qaly = NA,
                                                       deaths = NA,
                                                       hosp = NA,
                                                       prod = NA,
                                                       runtime = NA,
                                                       cost_abx = sum(abx_cost),
                                                       cost_consult = sum(consult_cost),
                                                       cost_test = sum(test_cost),
                                                       cost_hospital = NA,
                                                       cost_prod = NA
                                                     ) %>%
                                                     mutate(
                                                       cost_healthcare = cost_abx + cost_test + cost_consult
                                                     )
                                                   
                                                   bind_rows(dat, careseek_aggr)
                                                 },
                                                 current_year, w,
                                                 .options = furrr_options(
                                                   seed = TRUE,
                                                   chunk_size = 40
                                                 )),
               microsim_abxcon = future_map2(microsim_abxcon, data$careseek,
                                             function(dat, careseek, current_year, current_week){
                                               careseek_abx <- careseek %>%
                                                 filter(abx_type != "none") %>%
                                                 mutate(ddd_perday = ab_ddd(abx_type),
                                                        ab_days = abx_days * ddd_perday) %>%
                                                 rename(abx = abx_type) %>%
                                                 group_by(abx) %>%
                                                 summarise(ddd = sum(ab_days), .groups = "drop") %>%
                                                 mutate(year = current_year,
                                                        week = current_week)
                                               
                                               bind_rows(dat, careseek_abx)
                                             },
                                             current_year, w,
                                             .options = furrr_options(
                                               seed = TRUE,
                                               chunk_size = 40
                                             )))
      
      
      
      
      
      ## Annual demographic changes ----------------------------------------------
      
      
      if(w == ceiling(length(data$incuded_weeks[[1]]) / week_interval)) {
        #store microsim results
        # saveRDS(runsim, 
        #         file = paste0(save_dir,"/raw-microsim-", current_year,"-",today(),"-",hour(now()),minute(now()),"-rep-",replication,".RDS"))
        #rm(runsim)
        #
        
        ## DEMOGRAPHIC CHANGES ##
        #get excess mortality of the current year, due to long-term effects of hospitalizations
        # excess_mort <- future_pmap(list(nodes = data$nodes,
        #                          mortality_data = data$mortality,
        #                          microsim_hosp = results$microsim_hospitalized,
        #                          long_term_risk = data$longterm),
        #                     run_excess_mortality,
        #                     current_year = current_year,
        #                     .options = furrr_options(seed = TRUE))
        
        #increase the year by 1
        current_year <- current_year + 1
        
        #mortality
        mort_list <- base_mort <- future_map2(data$nodes, data$mortality, run_base_mortality, current_year, .options = furrr_options(seed = TRUE))
        #mort_list <- map2(base_mort, excess_mort, ~c(.x,.y))
        
        # apply population changes (in order: mortality, ageing, births, migration, employment, vaccination)
        data <- data %>%
          mutate(nodes = future_pmap(list(nodes,
                                          mort_list,
                                          fertility,
                                          migration,
                                          employment,
                                          vaccination,
                                          utility_data),
                                     function(nodes, 
                                              mort_list, 
                                              fertility, 
                                              migration, 
                                              employment, 
                                              vaccination,
                                              utility_data,
                                              current_year){
                                       nodes %>%
                                         node_remove(mort_list) %>%
                                         increase_age() %>%
                                         births(fertility, current_year) %>%
                                         run_migration(migration, current_year) #%>%
                                       #calc_employment(employment) %>%
                                       #run_influenza_vaccine(vaccination) %>%
                                       #run_base_utility(utility_data)
                                     },
                                     current_year = current_year,
                                     .options = furrr_options(seed = TRUE)))
        
        
        ## AMR CALCULATIONS ##
        current_amr <- calc_amr(data, results, current_year)
        
        # store
        results %<>% mutate(
          amr_rates = pmap(list(country, strategy, iteration, amr_rates),
                           ~add_row(..4, current_amr %>%
                                      filter(country == ..1,
                                             strategy == ..2,
                                             iteration == ..3) %>%
                                      select(year, amr))))
        
        
        ## RECORD RESULTS ##
        # population pyramid
        results$pyramid <- map2(results$pyramid, data$nodes, create_pyramid, current_year)
        
        # mortality of the year
        # results$mortality <- map2(results$mortality, 
        #                           map2(base_mort, 
        #                                excess_mort, 
        #                                ~tibble(year = current_year,
        #                                        base = length(.x),
        #                                        excess = length(.y),
        #                                        overlap = length(.x[.x %in% .y]))), 
        #                           bind_rows)
        
      }
    }
  }
  
  
  print(Sys.time()-p)
  #plan(sequential)
  
  # Process and save results ------------------------------------------------
  
  
  
  ## COST EFFECTIVENESS ##
  print("start ce result registration")
  write_rds(results, str_c(save_dir, "/results_",iter,"_", Sys.Date(),".RDS"), compress = "gz", version = 3)
  write_rds(data, str_c(save_dir, "/datafile_",iter,"_", Sys.Date(),".RDS"), compress = "gz", version = 3)
  rm(list=ls())
  return("finish")
}




## Set seed ----------------------------------------------------------------

RNGkind("L'Ecuyer-CMRG") #make sure random numbers work in cluster
set.seed(2222)


## Initiate parallel computing ---------------------------------------------


registerDoFuture()
options(future.globals.maxSize = 1024*1024^2)
#plan(list(tweak(multisession, workers = 2), tweak(multisession, workers = 3)))
plan(multisession, workers = n_cores)

registerDoRNG(seed = 2222)
libs <- .libPaths()

# set to use checkpoint in all R instances
paths <- foreach(i = 1:n_cores) %dopar% {
  .libPaths(libs)
  paste0("worker: ",Sys.getpid()," libpath:", .libPaths())
}

# run model iterations
print("start MC")
foreach(i = 1:n_iter,
        .options.future = list(chunk_size = 20)) %dorng% {
  print(i)
  model_run <- run_crp_comparison(i, params)
  rm(model_run)
}

# for(i in 1:32)  {
#   print(i)
#   run_crp_comparison(i, params)
# }

plan(sequential)

#record total life years of the year
# aggregated_results <- results %>%
#   select(country, strategy, iteration, microsim_aggregated) %>%
#   unnest(cols = c(microsim_aggregated))
# 
# newres_periteration <- results %>%
#   select(country, strategy, iteration, discount_health, discount_economic, ly) %>%
#   unnest(cols = c(ly)) %>%
#   left_join(aggregated_results, by = c("country", "strategy", "iteration", "year", "week")) %>%
#   mutate(lifeyear = lifeyear.x + if_else(is.na(lifeyear.y), 0, lifeyear.y),
#          qaly = qaly.x + if_else(is.na(qaly.y), 0, qaly.y))
# 
# 
# newres_periteration %<>%
#   mutate(
#          across(.cols = starts_with("cost_"),
#                 .fns = ~.x * 1/((1+discount_economic)^(year - start_year)),
#                 .names  = "disc_{.col}"),
#          across(.cols = c("lifeyear", "qaly"),
#                 .fns = ~.x * 1/((1+discount_health)^(year - start_year)),
#                 .names = "disc_{.col}")) %>%
#   select(-qaly.x, -qaly.y, -lifeyear.x, -lifeyear.y, -runtime, -discount_health, -discount_economic, -year, -week) %>%
#   group_by(country, strategy, iteration) %>%
#   summarise(across(.cols = everything(),
#                    ~sum(.x, na.rm = TRUE)),
#             .groups = "drop")
# 
# 
# newres_total <- newres_periteration %>%
#   select(-iteration) %>%
#   group_by(country, strategy) %>%
#   summarise(across(.cols = everything(),
#                    .fns = list(mean = mean,
#                                low = ~quantile(.x, .025),
#                                high = ~quantile(.x, .975)),
#                    .names = "{.fn}_{.col}"),
#             .groups = "keep")
# 
# 
# 
# 
# abx_consum <- results %>%
#   select(country, strategy, iteration, microsim_abxcon) %>%
#   unnest(microsim_abxcon) %>%
#   group_by(country, strategy, iteration, year, abx) %>%
#   summarise(ddd = sum(ddd),
#             ddd_cor = (sum(ddd) * .1) / 365.25,
#             .groups = "drop") %>%
#   group_by(country, strategy, year, abx) %>%
#   summarise(across(.cols = starts_with("dd"), .fns = list(mean = mean,
#                                             low = ~quantile(.x, .025),
#                                             high = ~quantile(.x, .975)),
#                    .names = "{.fn}_{.col}"))
# 
# amr_prsp <- results %>%
#   select(country, strategy, iteration, amr_rates) %>%
#   unnest(amr_rates) %>%
#   select(-year) %>%
#   unnest(amr) %>%
#   filter(abx == "BSP") %>%
#   group_by(country, strategy, year) %>%
#   rename(amr = amr_value) %>%
#   summarise(across(.cols = c("reduction","amr"),
#                    .fns = list(med = ~median(.x),
#                                lo = ~quantile(.x, .025),
#                                hi = ~quantile(.x, .975))),
#             mean_elasticity = mean(elasticity))




# 
# ms_res <- aggregated_results %>%
#   group_by(country, year, strategy) %>%
#   summarise(incidence = sum(incidence),
#             lifeyear_ms = sum(lifeyear),
#             costs_ms = sum(costs),
#             qaly_ms = sum(qaly),
#             .groups = "drop")
# 
# notms_res <- results %>%
#   select(country, strategy, ly) %>%
#   mutate(ly = map(ly, value)) %>%
#   unnest(cols = c(ly)) %>%
#   group_by(country, year, strategy) %>%
#   summarise(lifeyear_notms = sum(lifeyear),
#             qaly_notms = sum(qaly),
#             .groups = "drop")
#   
# results_ce <- 
#   left_join(ms_res, notms_res, by = c("country", "strategy", "year")) %>%
#   mutate(lifeyear = lifeyear_ms + lifeyear_notms,
#          qaly = qaly_ms + qaly_notms,
#          costs = costs_ms,
#          dis_clin = 1/((1+discount_rate_clin)^(year - start_year)),
#          dis_cost = 1/((1+discount_rate_cost)^(year - start_year)),
#          lifeyear_dis = lifeyear * dis_clin,
#          qaly_dis = qaly * dis_clin,
#          costs_dis = costs * dis_cost) %>%
#   select(-lifeyear_ms, -lifeyear_notms, -qaly_ms, -qaly_notms, -costs_ms, -dis_clin, -dis_cost) %>%
#   pivot_wider(names_from = "strategy", values_from = c("incidence", "lifeyear", "qaly", "costs", "lifeyear_dis", "qaly_dis", "costs_dis"))
# 
# pyramid_results <- results %>%
#   select(country, strategy, pyramid) %>%
#   unnest(cols = c(pyramid))
#   
#   
#   



