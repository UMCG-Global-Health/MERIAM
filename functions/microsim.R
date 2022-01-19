#import parameters
microsim_load_params <- function(filename, strategy){
  #load yaml file from data_input
    import <- read_yaml(filename)
    
    for(i in 1:length(import)){
      #if additional data is needed, import that here
      import[[i]] <- modify_if(import[[i]],
                ~is.character(.x$data),
                ~list("function" = .x$`function`,
                      "dist" = .x$dist,
                      "value" = .x$value,
                      "data" = as_tibble(read.csv(str_c("data_input/microsim_data/",.x$data))),
                      "dependency" = .x$dependency,
                      "reference" = .x$reference))
      
      #if a function should be imported, import that here
      import[[i]] <- modify_if(import[[i]],
                ~isTRUE(.x$`function`),
                ~list("function" = .x$`function`,
                      "dist" = .x$dist,
                      "value" = source(paste0("data_input/microsim_data/microsim_functions/", .x$value))$value,
                      "data" = .x$data,
                      "dependency" = .x$dependency,
                      "reference" = .x$reference))
    }
  
  
  #make a list with  one group for each strategy
  
  #first filter out the transition probabilities that are valid for all strategies
  all <- pluck(import, "all")
  output <- list()
  if(!is.null(import[strategy])){
    test_specific <- pluck(import, strategy)
    output <- append(all, test_specific)
  }else{
    output <- all
  }

  return(output)
}

#generate probabilities
microsim_generate_probs <- function(prob_list, nodes, n_cycles, probabilistic = FALSE) {
  
  #the actual number of cycles is one less than the number of days of the microsim
  n_cycles <- n_cycles - 1
  
  #create array with probabilities for all nodes
  prob_array <- array(data = NA,
                      dim = c(length(prob_list), n_cycles, nrow(nodes)),
                      dimnames = list(names(prob_list), 1:n_cycles, nodes$id))
  
  if(probabilistic == FALSE){
    for( i in 1: length(prob_list)){
    if(prob_list[[i]]$`function` == TRUE){
      #if the transition probability is based on a function, read in the function
      #this is done for each individual separately (using mapply)

      prob_array[i,1:n_cycles,] <- mapply(prob_list[[i]]$value,
                                          n_cycles, 
                                          nodes$sex, 
                                          nodes$age, 
                                          nodes$careseek_type, 
                                          MoreArgs = list(inputdata = prob_list[[i]]$data))

    
       
      }else{
        #if the transition probability is a static value, read in the value
        #this is done for all individuals simultaneously
        prob_array[i,1:n_cycles,] <- prob_list[[i]]$value
      }
    }
  } else if(probabilistic == TRUE){
    #probabilistic methods are not yet implemented
    return(warning("not implemented yet"))
  }
  
  return(prob_array)
}


#### parameters for testing purposes #####
# test <- create_data(n_nodes = 1000,
#                     country_list = c("nl"),
#                     strategy_list = c("base"),
#                     start_year = 2020,
#                     n_years = 1,
#                     included_weeks = c(40:52, 1:20),
#                     currency_year = 2019,
#                     productivity_method = "friction",
#                     use_corrected_mortality = TRUE,
#                     probabilistic = FALSE,
#                     iterations = iterations)
# 
# test %<>% 
#   #sample nodes that seek care
#   mutate(careseek = run_careseek(x = .,
#                                  child_multiply = ppas_child(),
#                                  year = 2020,
#                                  weeks = 1:20),
#          #add consult costs:
#          careseek = map2(careseek, test$consult_costs, 
#                                 cs_consult_costs),
#          #add tests:
#          careseek = map2(careseek, test$tests, cs_tests),
#          #add antibiotics
#          careseek = pmap(list(careseek, 
#                                      test$antibiotic_prescribing, 
#                                      test$antibiotic_types, 
#                                      test$consult_costs),
#                                 cs_antibiotics))
# 
# nodes <- test$careseek[[1]]
# params_probabilities <- microsim_generate_probs(prob_list = test$probabilities[[1]], 
#                                                    nodes = nodes,
#                                                    n_cycles =test$microsim_duration[[1]])
# 
# params_utilities <- test$utilities[[1]]
# 
# n_cycles <- test$microsim_duration[[1]]

#core microsim function
microsim_core <- function(nodes, params_probabilities, params_utilities, n_cycles) {
  
  ###### DATA IMPORT #######
  id <- nodes$id #IDs of modeled individuals
  node_utility <- nodes$uti_base - nodes$uti_dec
  
  #the actual number of cycles is one less than the number of days of the microsim
  n_cycles <- n_cycles - 1
  t <- 0 #current cycle
  cycle_length <- 1 #cycle length is one day
  
  n_ind <- length(id)
  
  n_states <- c("sick", "hospital", "icu", "healthy", "death")
  
  #import HRQoL data
  #first make a list with the base utility values
  q_list <- params_utilities$base_utilities
  
  #then create a matrix, where for each individual the base utility is given for the state "healthy"
  q_matrix <- matrix(data = q_list, 
                     nrow = n_ind, ncol = length(q_list),
                     byrow = TRUE,
                     dimnames = list(id, names(q_list)))
  q_matrix[,"q_healthy"] <- node_utility

  
  q_dec_list <- c("abx" = params_utilities$utility_decrements[[1]]$value)
  
  
  ##### NESTED FUNCTIONS #####
  
  ##calc transition probabilities
  calc_trans <- function(current_state, time_in_state, params_probabilities) {
    
    tis_index <- 1:length(time_in_state)
    
    prob <- mapply(function(time_in_state, tis_index, generated_probabilities){
      generated_probabilities[,time_in_state,tis_index]
      
    },time_in_state, tis_index, MoreArgs = list(generated_probabilities = params_probabilities))
    
    states <- c("sick", "hospital", "icu", "healthy", "death")
    n_states <- length(states)
    n_ind <- length(current_state)
    #p_create is a matrix which contains for each individual the probability of ending up in a certain state
    p_create <- matrix(NA, n_states, n_ind, dimnames = list(states,
                                                            paste("ind", 1:n_ind, sep = " ")))
    
    #read in the applicable transition probabilities (based on the nodes' current state) from params_probabilities
    p_create[,current_state == "sick"] <-     matrix(data = c(1-prob['p_sc',current_state == "sick"]-prob['p_sh',current_state == "sick"]-prob['p_si',current_state == "sick"]-prob['p_sd',current_state == "sick"], prob['p_sh',current_state == "sick"], prob['p_si',current_state == "sick"], prob['p_sc',current_state == "sick"], prob['p_sd',current_state == "sick"]),
                                                     nrow = n_states, 
                                                     ncol = sum(current_state == "sick"),
                                                     byrow = TRUE)
    p_create[,current_state == "hospital"] <- matrix(data = c(prob['p_hs',current_state == "hospital"], (1-prob['p_hs',current_state == "hospital"]-prob['p_hc',current_state == "hospital"]-prob['p_hi',current_state == "hospital"]-prob['p_hd',current_state == "hospital"]), prob['p_hi',current_state == "hospital"], prob['p_hc',current_state == "hospital"], prob['p_hd',current_state == "hospital"]),
                                                     nrow = n_states,
                                                     ncol = sum(current_state == "hospital"),
                                                     byrow = TRUE)
    p_create[,current_state == "icu"] <-      matrix(data = c(prob['p_is',current_state == "icu"], prob['p_ih',current_state == "icu"], (1-prob['p_is',current_state == "icu"]-prob['p_ih',current_state == "icu"]-prob['p_ic',current_state == "icu"]-prob['p_id',current_state == "icu"]), prob['p_ic',current_state == "icu"], prob['p_id',current_state == "icu"]),
                                                     nrow = n_states,
                                                     ncol = sum(current_state == "icu"),
                                                     byrow = TRUE)
    p_create[,current_state == "healthy"] <-  matrix(data = rep(c(0,0,0,1,0), sum(current_state == "healthy")),
                                                     nrow = n_states,
                                                     ncol = sum(current_state == "healthy"),
                                                     byrow = FALSE)
    p_create[,current_state == "death"] <-    matrix(data = rep(c(0,0,0,0,1), sum(current_state == "death")),
                                                     nrow = n_states,
                                                     ncol = sum(current_state == "death"),
                                                     byrow = FALSE)
    #check if the sums of probabilities add up to 1, then return
    ifelse(colSums(p_create) == 1, return(t(p_create)), print("Probabilities do not sum to 1")) # return the transition probabilities or produce an error
  }
  
  #function to count the time that a patient is in the current state
  time_in_state <- function(states, cycle, tis_prev){
    out <- mapply(function(current_state, prev_state, tis_prev){
      if(current_state == prev_state){
        tis_prev + 1
      }else{
        1
      }
      
    }, states[,cycle+1], states[,cycle], tis_prev)
    return(out)
  }
  
  ##calc quality of life
  calc_qald <- function(current_state, current_abx, q_matrix, q_dec_list){
    out <- mapply(function(id, current_state, current_abx){
      if(is.na(current_abx)){
        uti <- dec_abx <- 0
      } else{
        uti <- switch(current_state,
                      "sick" = q_matrix[[id,"q_sick"]],
                      "hospital" = q_matrix[[id,"q_hospital"]],
                      "icu" = q_matrix[[id,"q_icu"]],
                      "healthy" = q_matrix[[id,"q_healthy"]],
                      "death" = q_matrix[[id,"q_death"]])
        
        dec_abx <- ifelse(current_abx == "none", 0, q_dec_list[[1]])
      }
      
      as.double(uti + dec_abx)
      
    }, names(current_state), current_state, current_abx, SIMPLIFY = TRUE)
    
    return(unlist(out))
  }
  
  # calc_cost <- function(current_state, current_abx, abx_delay, abx_costs){
  #   mapply(function(current_state, current_abx, abx_delay, abx_costs){
  #     #abx costs
  #     if((current_abx != "none" | is.na(current_abx)) && (abx_delay == 0 | is.na(abx_delay))){
  #       abx_ret <- abx_costs
  #     }
  #     else{
  #       abx_ret <- 0
  #     }
  #     
  #     abx_ret
  #   }, current_state, current_abx, abx_delay, abx_costs,
  #   SIMPLIFY = TRUE)
  # }
  
  
  ##function for antibiotic treatment type
  calc_abx_type <- function(type, days, state){
    mapply(function(type, days, state){
      if(state == "death"){
        return(NA)
      } else if(days == 0){
        return("none")
      } else{
        return(type)
      }
    }, type, days, state)
  }
  
  ##count down delay in case of delayed prescription
  calc_abx_delay <- function(delay, state){
    mapply(function(timer, state) {
      if(state == "death"){
        return(NA)
      } else if(timer > 0){
        return(timer-1)
      } else{
        return(0)  
      }
    }, delay, state, SIMPLIFY = TRUE)
  }
  
  ##count down number of days that patients use antibiotics
  calc_abx_days <- function(days, delay, state){
    mapply(function(days, delay, state){
      if(state == "death"){
        return(NA)
      }else if(days == 999){
        #some abx are continued until patient is cured, code 999 is used for this
        return(ifelse(state != "sick", 0, 999))
      }else if((delay == 0 | is.na(delay)) && days > 0){
        return(days-1)
      } else if(delay > 0 & !is.na(delay)){
        return(days)
      } else{
        return(0)
      }
    }, days, delay, state)
  }
  
  
  ##### END NESTED FUNCTIONS #####
  
  #continue setting up the model
  
  #initialize all matrices (all are the same)
  ##create matrices to store results
  m_states <- m_tis <- m_costs <- m_effects <- m_abx_delay <- m_abx_days <- m_abx_type <- matrix(nrow = n_ind, 
                                                                                                 ncol = n_cycles + 1,
                                                                                                 dimnames = list(id,
                                                                                                                 paste("cycle", 0:(n_cycles), sep = " ")))
  
  ##initiatie all patients in sick state
  m_states[1:n_ind, 1] <- "sick"
  
  ##initialize matrix to record time in current state
  m_tis[, 1] <- 1
  
  ##antibiotics import
  ###antibiotic type
  m_abx_type[1:n_ind, 1]  <- nodes$abx_type
  
  ###delayed prescriptions, also calculating the maximum number of days of the delay
  m_abx_delay[1:n_ind, 1]  <- nodes$abx_delay
  max_delay <- max(nodes$abx_delay) + 1
  
  ###prescribed number of days of the antibiotic
  m_abx_days[1:n_ind, 1]  <- nodes$abx_days
  
  ###calculate qald's associated with initiated health state
  m_effects[1:n_ind, 1] <- calc_qald(m_states[1:n_ind, 1], m_abx_type[1:n_ind, 1], q_matrix, q_dec_list)
  
  ###costs can be added later, but are set to 0 for now and calculated afterwards
  m_costs[1:n_ind, 1:n_cycles + 1] <- 0 #calc_cost(m_states[1:n_ind, 1], m_abx_type[1:n_ind, 1], m_abx_delay[1:n_ind, 1], nodes$abx_cost)
  
  ####loop through time
  for(t in 1:n_cycles){
    #FUNCTION TO CALCULATE TRANSITION PROBABILITIES
    p_trans <- calc_trans(m_states[, t], 
                          m_tis[, t], #time_in_state(m_states, t),
                          params_probabilities)     
    
    #SAMPLE NEXT STATE USING TRANSITION PROBABILITIES
    m_states[, t+1] <- samplev(probs = p_trans, m = 1)    
    
    #RECORD HOW LONG PERSON IS IN STATE
    m_tis[, t+1] <- time_in_state(m_states, t, m_tis[, t])
    
    #SET ANTIBIOTIC FOR NEXT CYCLE
    m_abx_type[, t+1] <- calc_abx_type(m_abx_type[, t], m_abx_days[, t], m_states[, t + 1]) 
    
    #SET REMAINING NUMBER OF DAYS ON ANTIBIOTIC
    m_abx_days[, t+1] <- calc_abx_days(m_abx_days[, t], m_abx_delay[, t], m_states[, t])
    
    #IN CASE OF A DELAYED PRESCRIPTION, COUNT DOWN DELAY (ONLY EXECUTED IF t IS SMALLER THAN THE MAXIMUM DELAY)
    if(t < max_delay){m_abx_delay[, t+1] <- calc_abx_delay(m_abx_delay[, t],m_states[, t]) }  
    
    #CURRENTLY COSTS ARE NOT INCLUDED IN MICROSIMULATION, BUT CALCULATED AFTERWARDS
    #m_costs[, t+1] <- calc_cost(m_states[, t+1], m_abx_type[, t+1], m_abx_delay[, t+1], nodes$abx_cost)
    
    m_effects[, t+1] <- calc_qald(m_states[, t+1], m_abx_type[, t+1], q_matrix, q_dec_list)     #CALCULATE QUALITY OF LIFE BASED ON PATIENT HEALTH STATE
    
  } ####close loop of time
  

  ##### ---  Combine results as output  ---  #####
  c_costs <- rowSums(m_costs, na.rm = TRUE)
  c_uti <- rowSums(m_effects, na.rm = TRUE)
  c_mort <- id[sapply(m_states[,n_cycles], function(x) x == "death")]
  
  #### ---- RETURN A LIST WITH OUTPUT DATA ----    ####
  output <- list(c_mort, c_costs, c_uti, m_states, m_abx_type, m_abx_delay, m_abx_days)
  names(output) <- c("mortality",
                     "costs",
                     "utilities",
                     "states",
                     "abx",
                     "abx_delay",
                     "abx_days")
  return(output)
}

#function that executes the microsimulation and proceses the results
proces_microsim <- function(nodes, probabilities, utilities, labour_data, consult_cost_data, microsim_cycles, current_year, week, .useGroups){
  ##start with running the microsim
  t <- Sys.time()
  if(nrow(nodes) <= 1000 | .useGroups == FALSE){
    #if number of nodes is less than 1000 don't run in parallel
    generated_probabilities <- microsim_generate_probs(prob_list = probabilities, 
                                                       nodes = nodes,
                                                       n_cycles = microsim_cycles)
    
    sims <- microsim_core(nodes, generated_probabilities, utilities, microsim_cycles)
  } else{
    #if there are more than 1000 individuals to simulate, split the workload over the number of cores available
    
    #create groups to split workload and apply to nodes
    n_groups <- availableCores() * 2
    
    nodes %<>% tibble::add_column(group = rep(1:n_groups, length.out = nrow(.)))
    
    #run microsim using parallel foreach
    sims <- foreach(i=1:n_groups, 
                    .inorder = FALSE, 
                    .combine = combine_results) %dorng%
      {
        
        #select nodes of current group
        nodes_subgroup <- nodes %>% 
          filter(group == i) %>%
          select(-group)
        
        generated_probabilities <- microsim_generate_probs(prob_list = probabilities, 
                                                           nodes = nodes_subgroup,
                                                           n_cycles = microsim_cycles)
        #run core microsim with split data
        microsim_core(nodes_subgroup, generated_probabilities, utilities, microsim_cycles)
        
      }
    nodes %<>% select(-group)
  }
  runtime <- Sys.time() - t
  
  ## load cost items for calculation
  costs_hospital_day <- consult_cost_data %>%
    filter(item == "hospital stay ward") %>%
    pull(cost)
  
  ### icu not yet implemented
  # costs_hospital_icu <- consult_cost_data %>%
  #   filter(item == "hospital stay icu") %>%
  #   pull(cost)
  
  ## process the results of the microsim
  results_microsim_current <- tibble::enframe(sims$utilities,
                                      name = "id", 
                                      value = "utilities") %>%
    mutate(id = as.double(id),
           sim_cost = sims$costs) %>%
    left_join(nodes, by = "id") %>%
    tibble::add_column(final_state = sims$states[,28],
               sickdays = apply(sims$states, MARGIN = 1, function(x) sum(str_detect(x, "healthy", negate = TRUE)) - sum(str_detect(x, "death", negate = FALSE))),
               daysalive = apply(sims$states, MARGIN = 1, function(x) sum(str_detect(x, "death", negate = TRUE))),
               hospital_los = apply(sims$states, MARGIN = 1, function(x) sum(str_detect(x, "hospital")))) %>%
    mutate(utilities = utilities/365.25,
           ly = daysalive/365.25,
           mort = if_else(final_state == "death", 1, 0),
           hos = if_else(hospital_los > 0, 1, 0),
           hos_costs = hospital_los * costs_hospital_day,
           cost_prod = calc_prod_loss(final_state, sickdays, age, empl, productivity_data = labour_data),
           week = week) 
  
  #create tibble with aggregated results
  results_microsim_aggr <- results_microsim_current %>%
    group_by(week) %>%
    summarise(incidence = n(),
              cost_abx= sum(abx_cost),
              cost_consult = sum(consult_cost),
              cost_test = sum(test_cost),
              cost_hospital = sum(hos_costs),
              cost_microsim = sum(sim_cost),
              lifeyear = sum(ly),
              qaly = sum(utilities),
              deaths = sum(mort),
              hosp = sum(hos),
              cost_prod = sum(cost_prod),
              .groups = "drop") %>%
    mutate(runtime = runtime,
           cost_healthcare = sum(cost_abx, cost_consult, cost_test, cost_microsim, cost_hospital),
           year = current_year) 
  
  #create tibble with hospitalized patients
  results_microsim_hospitalized <- results_microsim_current %>%
    filter(hospital_los > 0) %>%
    select(id, sex, age) %>%
    mutate(week = week,
           year = current_year)
  
  #calculate total abx consumption
  abx_consumption_vec <- mapply(function(abx, delay){
                            #remove all items where the abx is "none", the delay > 0, or the patient has died
                            if((abx != "none" | is.na(abx)) && (delay == 0 | is.na(delay))){
                              #if true: return the antibiotic
                              abx
                            }
                            else{
                              #if false: return NA
                              NA
                            }
                          }, as.vector(sims$abx), as.vector(sims$abx_delay),
                          USE.NAMES = FALSE)
  
  abx_consumption <-  tibble(abx = abx_consumption_vec) %>%
                      remove_missing(na.rm = TRUE) %>%
                      group_by(abx) %>%
                      summarise(count = n(), .groups = "drop") %>%
                      mutate(ddd = ab_ddd(abx)*count,
                             year = current_year,
                             week = week) %>%
                      select(-count)
  
  #create Markov trace (plot)
  d_states <- data.frame(sims$states, row.names = rownames(sims$states))
  d_states$id <- rownames(d_states)
  markov_trace <- d_states %>% 
    pivot_longer(1:28, names_to = "cycle", names_prefix = "cycle.", values_to = "state") %>%
    mutate(cycle = as.numeric(cycle),
           state = factor(state, levels = c("sick", "hospital", "icu", "healthy", "death"))) %>%
    group_by(state, cycle) %>%
    count() %>%
    ggplot(aes(x = cycle, y = n, color = state)) +
      geom_line(size = 1.2)
  
    
  
  #return data
  output <- list(raw = sims, 
                 hospitalized = results_microsim_hospitalized, 
                 abx_consumption = abx_consumption,
                 aggregated = results_microsim_aggr,
                 plot = markov_trace)
  
  return(output)
}

#wrapper function to use purrr to perform the microsim
microsim <- function(data, current_year, week, parallel_mode = "map") {
  if(parallel_mode ==  "map"){
    sim_result <- future_pmap(list(data$careseek, 
                                   data$probabilities, 
                                   data$utilities, 
                                   data$labour,
                                   data$consult_costdata,
                                   data$microsim_duration),
                              proces_microsim,
                              current_year, week, .useGroups = FALSE,
                              .options = furrr_options(seed = TRUE))
  } else if(parallel_mode == "both") {
    sim_result <- future_pmap(list(data$careseek, 
                                   data$probabilities, 
                                   data$utilities, 
                                   data$labour,
                                   data$consult_costdata,
                                   data$microsim_duration),
                              proces_microsim,
                              current_year, week, .useGroups = TRUE,
                             .options = furrr_options(seed = TRUE))
  } else {
    sim_result <- pmap(list(data$careseek, 
                            data$probabilities, 
                            data$utilities, 
                            data$labour,
                            data$consult_costdata,
                            data$microsim_duration),
                       proces_microsim,
                       current_year, week, .useGroups = TRUE)
  }

  
  
  
  return(transpose(sim_result))
}

####END CORE MICROSIM FUNCTIONS####

#function to combine results of foreach loop
combine_results <- function(x,y){
  if(exists("x") ==  FALSE){
    x <- list("strategy" = integer(),
              "mortality" = integer(),
              "costs" = integer(),
              "utilities" = integer(),
              "states" = character(),
              "abx" = character(),
              "abx_delay" = integer(),
              "abx_days" = integer())
  }
  
  for(i in 1:length(x)){
    if(is.matrix(y[[i]])){
      x[[i]] <- rbind(x[[i]], y[[i]])
    } else
      x[[i]] <- c(x[[i]], y[[i]])
  }
  return(x)
}

#utility changes after hospitalization
change_posthospital_utility <- function(nodes, microsim_hospitalized, long_term) {
  ids <- microsim_hospitalized %>%
    filter(age >= 65) %>%
    pull(id)
  
  utility_decrements <- nodes$uti_dec[nodes$id %in% ids] + long_term$utility_decrement$cap$value
  names(utility_decrements) <- ids
  
  return(run_utility_decrement(nodes, utility_decrements))
}



