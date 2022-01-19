#population script to model population, which is in the form of a tibble

# library(dplyr)
# library(forcats)
# library(tidyr)
# library(stringr)
# library(foreach)
# library(doFuture)

#function to create nodes
##init means initialize node list - otherwise the new nodes will be added to an existing list

node_create <- function(data = NULL, year = 2000, n = 0, active = 1, sex = 0, age = 0, empl = "employed", uti_base = NA, uti_dec = 0, vac_inf = 0, migration = FALSE){
  if(n == 0){
    #if no additional nodes are asked, return data as is
    return(data)
  } else if(n > 0) {
    id_nr <- sample.int(99999999, n, replace = FALSE)
    #ids are created as follows: yyaasRRRRRRR (y = year created, aa = age, s = sex, R = id_nr, random)
    #if node is created by migration: node_id sex = 3 (male) or 4 (female)
    new_nodes <- tibble(
      id = id_nr + 100000000*(sex+ifelse(migration,2,0))+ 1000000000*age + (year-2000)*100000000000,
      active = active,
      sex = sex,
      age = age,
      empl = empl,
      vac_inf = 0,
      uti_base = uti_base,
      uti_dec = uti_dec
    )
    
    if(is.null(data)){
      return(new_nodes)
    } else {
      list_out <- bind_rows(data, new_nodes)
      return(list_out)
    }
  }
    warning("node create failed")
}

#function used to deactivate nodes
node_remove <- function(nodes, id_list){
  nodes <- nodes %>% mutate(active = ifelse(id %in% id_list, 0, active))
  nodes
}

#age distribution

agedist <- function(age_data, length) {
  age_p <- age_data %>%
    arrange(age) %>%
    pull(ageprop)
  
  out <- sample(0:99, size = length, prob = age_p, replace = TRUE) %>%
    sort()
  return(out)
  
}

#sex distribution

sexdist <- function(sex_data, age_dist){
  #for all ages for all individual (from age_dist), generate the probability of being female (sex_data[[(.x + 1),2]])
  #then use this to sample 1 (female) or 0 (male) using a binomial distribution
  map_dbl(age_dist, ~rbinom(1, 1, sex_data[[(.x + 1),2]]))
}

#increase age

increase_age <- function(x){
  
  x %<>% mutate(age = age+1)
  return(x)
}

#births
##calculate the number of babies that are born. Population alive * (number of babies born/total population in the previous year)
births <- function(nodes, fertility_rates, current_year){
  fertility <- fertility_rates %>% 
    filter(time == current_year) %>% 
    pull(fertility)
  
  #old way of calculating number of babies
  #n_babies <- round(nrow(filter(nodes, active == 1))*fertility)
  
  #calculate the fertile population (aged 15 - 45 and alive)
  fert_pop <- nodes %>%
    filter(age >= 15,
           age <= 45,
           active == 1) %>%
    nrow()

  n_babies <- round(fert_pop * fertility)

  
  babies <- node_create(data = nodes,
                           year = current_year,
                           n = n_babies, 
                           active = 1, 
                           empl = "none",
                           sex = rbinom(sum(n_babies), 1, .5), 
                           age = 0)
  return(babies)
}

#migration

run_migration <- function(nodes, migration_data, year) {
  ##calculate total number of people alive
  list_functions <- c("node_create", "node_remove")
  
  #calculate for each strategy arm, the number of people alive
  n_active <- nodes %>%
      filter(active == 1) %>%
      pull(active) %>%
      sum()
  
  ##loop through age categories and sexes to create (immigration) or remove (emigration) nodes 
  migration_list <- foreach (a=rep(0:149,times = 2), s = rep(c(rep(0,150),rep(1,150))), .combine = "rbind", .export = list_functions) %dorng% {
    if(a < 100){
      #only data for age <= 99 is available, so check whether a <= 99
      node_select <- nodes %>% filter(age == a, sex == s) #select people age = a and sex = s 
      mig_rate <- migration_data %>% #select migration data current age and year
        filter(time == year, age == a, sex == s) %>%
        pull(rate)
      mig_n <- round(mig_rate*n_active) #number of people migrating (positive immigration, negative emigration)
      if(mig_n >= 1){
        #create nodes in case of immigration
        node_create(node_select, 
                    year = year,
                    n = mig_n,
                    sex = s, 
                    age = a,
                    migration = TRUE)
      } else if(mig_n <= -1) {
        #remove nodes in case of emigration
        node_remove(node_select, 
                    sample(node_select$id[node_select$active == 1], 
                           size = (mig_n*-1), 
                           replace = FALSE))
      } else {
        #if there is no immigration or emigration, return node_list without any changes
        node_select
      }
    }else{
      #if a > 99, return data as is
      nodes %>% filter(age == a, sex == s)
    }
  }
  return(as_tibble(migration_list))
}

#mortality

run_base_mortality <- function(nodes, mortality_data, year){
  #for all age categories, check mortality probability and make nodes inactive
    mortality_list <- foreach (a=rep(0:98,times = 2), s = rep(c(rep(0,99),rep(1,99))), .combine = "c", .packages = "dplyr") %dorng% {
      node_ids <- nodes %>%
        filter(active == 1,
               age == a,
               sex == s) %>%
        pull(id)
      
      mort_p <- mortality_data %>%
        filter(time == year, sex == s, age == a) %>% 
        pull(values)
      
      
      mort_n <- round(mort_p * length(node_ids))
      #calculate probability weights of mortality
      sample(node_ids, size = mort_n, replace = FALSE)

    }
  
  cent <- nodes %>%
    filter(age >= 98,
           active == 1) %>%
    pull(id)
    
  c(mortality_list,cent)
  
}

run_excess_mortality <- function(nodes, mortality_data, microsim_hosp, long_term_risk, current_year, probabilistic = FALSE){

  ##select hospitalized patients in the past year, aged >= 65 years
  ms_hosp <- microsim_hosp %>%
    filter(year == current_year,
           age >= 65) %>%
    select(-year, -week)
  
  #effective rr is the value from literature as the mean
  if(probabilistic == FALSE){
    effective_rr <- long_term_risk$excess_mortality$cap$relative_risk
  } else if(mode == "probabilistic"){
    rr <- long_term_risk$excess_mortality$cap$relative_risk
    sd <- long_term_risk$excess_mortality$cap$relative_risk_sd
    if(long_term_risk$excess_mortality$cap$dist_function == "rlnorm"){
      fun <- rlnorm
    }
    effective_rr <- fun(1, log(rr), sd)
  }
  
  mortality_list <- foreach (a=rep(65:98,times = 2), s = rep(c(rep(0,34),rep(1,34))), .combine = "c") %dorng% {
    node_ids <- ms_hosp %>%
      filter(age == a,
             sex == s) %>%
      pull(id)
    
    base_p <- mortality_data %>%
      filter(time == current_year, sex == s, age == a) %>% 
      pull(values)
    
    risk_p <- base_p * effective_rr - base_p
    
    mort_n <- round(risk_p * length(node_ids))
 
    if(mort_n > 0 && risk_p <= 1){
      sample(node_ids, size = mort_n, replace = FALSE)
    } else if(mort_n > 0 && risk_p > 1){
      #if probability of dying is higher than 1, all nodes will be deactivated
      node_ids
    }
  }
  
  return(mortality_list)
}


#change utilities based on named vector
run_utility_decrement <- function(nodes, uti_dec){
  nodes$uti_dec[nodes$id %in% names(uti_dec)] <- abs(uti_dec) #absolute value, to make sure the decrement is always positive
  return(nodes)
}

#set utilities
run_base_utility <- function(nodes, utility_data){
  nodes_active <- nodes %>% filter(active == 1)
  nodes_inactive <- nodes %>% filter(active == 0)
  age_list <- nodes_active$age
  sex_list <- nodes_active$sex
  
  #select utility values based on location in the utility_data tibble
  nodes_active$uti_base <- map2_dbl(age_list, sex_list, ~utility_data[[(.y*100+.x+1),3]])
  nodes_inactive$uti_base <- NA
  
  return(bind_rows(nodes_active, nodes_inactive))
}

#vaccination status
run_influenza_vaccine <- function(nodes, data_vaccination){
  #read in the probability of influenza vaccination
  infl_prob <- data_vaccination %>%
    filter(vac == "influenza") %>%
    pull(value)
  
  #select nodes  >= 65 years
  pop65 <- nodes %>%
    filter(age >= 65) %>%
    mutate(vac_inf = sample(c(1,0), size = n(), replace = TRUE, prob = c(infl_prob, 1-infl_prob)))
  
  #first select nodes younger than 65, then add the (vaccinated) nodes >= 65
  output <- nodes %>%
    filter(age < 65) %>%
    bind_rows(pop65)
  
  return(output)
}

#function to record life years/QALYs from nodes not seeking care
record_lifeyears <- function(results, nodes, cycle_length, careseek = FALSE, included_weeks, current_year, week_interval, w, season = TRUE){

    week_order <- c(included_weeks, (1:52)[-included_weeks])
    max_w <- ceiling(length(included_weeks) / week_interval)
    corr_included_weeks <- unique(c(included_weeks,week_order[((max_w-1)*week_interval+1):(max_w*week_interval)]))
    
    
    if(careseek == FALSE && season == FALSE){
      #calculate portion of the year in which the microsim does not run
      #add 1.25 to include week 53 (i.e. to make sure each year equals 365.25 days)
      ly <- (length((1:52)[-corr_included_weeks])*7+1.25) / 365.25
      ly_cs <- 0
      #create a careseek$id, to avoid error later
      careseek <- list(id = 0)
      current_year <- current_year-1 #easier to summarise results later
    } else if(tibble::is_tibble(careseek) && season == TRUE){
      #generate included weeks
      weeks <- corr_included_weeks[((w-1)*week_interval+1):(w*week_interval)]
      #calculate the life years included
      ly <- (length(weeks)*7) / 365.25
      #for nodes seeking care, exclude number of days that they are in the microsimulation
      ly_cs <- (length(weeks)*7 - cycle_length) / 365.25
    } else{
      return(warning("error record_lifeyears(): careseek and season don't match"))
    }
    
    newresults <- nodes %>%
      filter(active == 1) %>%
      mutate(lifeyear = case_when(id %in% careseek$id ~ ly_cs, # careseeking nodes
                                  active == 1 ~ ly), #not careseeking nodes
             qaly = lifeyear * (uti_base - uti_dec),
             week = w) %>%
      group_by(week) %>%
      summarise(lifeyear = sum(lifeyear),
                qaly = sum(qaly),
                .groups = "drop") %>%
      mutate(year = current_year)
    
    return(add_row(results, newresults))

}

create_pyramid <- function(pyramid_tibble, nodes, current_year){
  pyramid_year <- nodes %>% 
    filter(active == 1) %>% 
    mutate(popGroup = cut(age, breaks = seq(0, 100, by = 10), labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-99"), include.lowest = TRUE),
           sex = if_else(sex == 1, "female", "male"),
           popGroup = factor(popGroup)) %>%
    group_by(popGroup, sex) %>%
    count() %>%
    mutate(n = ifelse(sex == "male", n*-1, n),
           year = as.double(current_year))
  
  bind_rows(pyramid_tibble, pyramid_year)
}

