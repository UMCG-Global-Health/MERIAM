library(aweek)
#Predict incidence -------------------------------------------------------
# depreciated
# #incidence_predict <- function(fitted_model, n_years, n_weeks, probabilistic = FALSE, repl = 1) {
#   #helper function to predict the incidence per week, using a supplied incidence model
#   incidence_predict_perweek <- function(week,  fitted_model, probabilistic, repl = 1) {
#     #convert weeks as needed for the prediction (first week = 3.5, then + 7 for each week), first week is always week 40
#     if(week < 25){
#       #convert weeks in January - May 
#       converted_week <- 94.5 + (7 * (week - 1))
#     } else if(week > 39) {
#       #convert weeks in October - December
#       converted_week <- 3.5 + (7 * (week - 40))
#     } else {
#       warning("week out of bounds incidence_predict")
#     }
#     
#     #read in the split date (after which the model starts decreasing)
#     split_date <- date2week(fitted_model$split, numeric = TRUE)
#     
#     if(split_date < 25){
#       #convert weeks in January - May 
#       converted_split <- 94.5 + 7 * (split_date - 1)
#     } else if(split_date > 39) {
#       #convert weeks in October - December
#       converted_split <- 3.5 + 7 * (split_date - 42)
#     } else {
#       warning("split error incidence_predict")
#     }
#     
#     if(converted_week < converted_split){
#       #use this if the epidemic is still increasing (before in model)
#       predict_value <- data.frame(dates.x = converted_week)
#       prediction <- predict(fitted_model$fit$before$model, predict_value, interval = "confidence")
#     } else {
#       #use this if the epidemic is decreasing (after in model)
#       converted_week <- converted_week - converted_split + 3.5
#       predict_value <- data.frame(dates.x = converted_week)
#       prediction <- predict(fitted_model$fit$after$model, predict_value, interval = "confidence")
#     }
#     #return the prediction, if prob = FALSE the point estimate, if prob = TRUE, a number (repl) of probabilistic estimates 
#     #assumed is a lognormal distribution
#     
#     if(probabilistic){
#       return(rlnorm(1, prediction[1], (prediction[3] - prediction[2]) / (2*1.96)))
#     } else{
#       return(exp(prediction[1]))
#     }
#   }
#   
#   name_list <- paste0(rep(n_years, each = length(n_weeks)), "-", rep(n_weeks, length(n_years))) 
#   # if(mode == "deterministic"){
#   #   out <- rep(sapply(n_weeks, incidence_predict_perweek, fitted_model), length(n_years))
#   #   names(out) <- name_list
#   # } else if(mode == "probabilistic") {
#   #   # out <- sapply(rep(n_weeks, length(n_years)), incidence_predict_perweek, fitted_model, mode, repl)
#   #   # colnames(out) <- name_list
#   #   return(warning("probabilistic incidence not implemented"))
#   # }
#   
#   out <- rep(sapply(n_weeks, incidence_predict_perweek, fitted_model, probabilistic), length(n_years))
#   names(out) <- name_list
# 
#   return(out)
# }







# Main consult function ---------------------------------------------------
# used to generate the index consultations with primary care physicians
# this function takes into account a rollout value, which means that the 
# diagnostics strategy is not rolled out in the country in one year
# but takes a couple of years, to be fully implemented
consult_gp <- function(data, weeks, current_year){

  careseek_data <- foreach(i = 1:nrow(data), 
                      .inorder = FALSE,
                      .export = c("data", "weeks", "current_year")) %dorng% {
                        #generate the nodes seeking care, using incidence data
                        careseek <- consult_sample(data$nodes[[i]], 
                                                   data$incidence[[i]],
                                                   current_year, 
                                                   weeks, 
                                                   exclude = NA)
                        
                        #get the rollout value for the current year
                        #this is the rate at which the new testing strategy is implemented
                        rollout_value <- data$rollout[[i]] %>%
                          filter(year == current_year) %>%
                          pull(rollout)
                        
                        if(rollout_value < 1){
                          # if the testing strategy is not rolled out completely
                          # use the base case scenario parameters for the other
                          # careseeking nodes
                          
                          # get the index of the base case for the current country
                          index_base <- data %>%
                            select(country, strategy, iteration) %>%
                            mutate(index = row_number()) %>%
                            filter(country == data$country[[i]],
                                   iteration <= data$iteration[[i]],
                                   strategy == "base") %>%
                            pull(index) %>%
                            max()
                            
                          
                          n_careseek <- nrow(careseek) # total number of nodes seeking care
                          n_test <- round(n_careseek * rollout_value) # number of nodes that are in the testing strategy
                          n_base <- n_careseek - n_test # number of nodes in the base case strategy
                          base_ids <- sample.int(n_careseek, size = n_base) #sample the nodes that will be treated as in base case
                          
                          #randomly assign nodes that are treated as in the base case
                          #and use costs, test and antibiotics as in the base case
                          careseek_base <- careseek %>%
                            slice(base_ids) %>% 
                            consult_costs(data$consult_costdata[[index_base]]) %>%
                            consult_tests(data$tests[[index_base]]) %>%
                            consult_antibiotics(data$antibiotic_prescribing[[index_base]], data$antibiotic_types[[index_base]], data$consult_costdata[[index_base]])
                          
                          #the other nodes are treated as in the testing scenario
                          careseek_test <- careseek %>%
                            slice(-base_ids) %>%
                            consult_costs(data$consult_costdata[[i]]) %>%
                            consult_tests(data$tests[[i]]) %>%
                            consult_antibiotics(data$antibiotic_prescribing[[i]], data$antibiotic_types[[i]], data$consult_costdata[[i]])
                          
                          #combine the results and add the country and strategy
                          #to be able to later identify the different groups
                          bind_rows(careseek_test, careseek_base)
                          
                          
                        } else{
                          #if a strategy is fully implemented, or if the base case is sampled
                          #use the values that are provided in the data tibble
                        
                          careseek %>%
                            consult_costs(data$consult_costdata[[i]]) %>%
                            consult_tests(data$tests[[i]]) %>%
                            consult_antibiotics(data$antibiotic_prescribing[[i]], data$antibiotic_types[[i]], data$consult_costdata[[i]])
                          
                        }
                      }
  
  #after the foreach loop, return the careseeking nodes in the data tibble
  data %>%
    mutate(careseek = careseek_data)


}

# Sample number of consults -----------------------------------------------

consult_sample <- function(nodes, incidence_data, current_year, current_weeks, exclude){
  #child multiply: depreciated
  #
  
  consulting_nodes <- foreach(i = 1:4, .inorder = FALSE, .combine = rbind) %do% {
    agegroup <- switch(i,
                       "00-04",
                       "05-14",
                       "15-64",
                       "65+")
    
    agelow <- switch(i,
                     0,
                     5,
                     15,
                     65)
    
    agehigh <- switch(i,
                      4,
                      14,
                      64,
                      100)
    
    #calculate incidence in weeks
    incidence <- incidence_data %>%
      filter(agecat == agegroup,
             week %in% current_weeks,
             year == current_year) %>%
      group_by(type) %>%
      summarise(incidence = sum(incidence) / 100000)
    
    #select nodes in age category
    node_select <- nodes %>%
      filter(active == 1,
             age >= agelow,
             age <= agehigh)
    
    sampled_nodes <- list()
    
    #number of individuals
    n_pop <- nrow(node_select)
    
    #create copy of node_select to sample from
    node_select_sample <- node_select
    
    #for all included types of disease (i.e. ari and ili) sample individuals to seek care
    for(j in 1:nrow(incidence)){
      sampled_node_ids <- sample(x = nrow(node_select_sample), 
                                 size = round(n_pop * incidence %>% slice(j) %>% pull(incidence)),
                                 replace = FALSE)
      
      sampled_nodes[[j]] <- node_select_sample %>% slice(sampled_node_ids) %>%
        mutate(careseek_type = incidence %>% slice(j) %>% pull(type))
      
      node_select_sample <- node_select_sample  %>% slice(-sampled_node_ids)
    }  
    
    #combine types of disease (i.e. ari and ili) to one tibble and return output
    bind_rows(sampled_nodes)
  }
  
  consulting_nodes %<>% mutate(costs = 0)
  
  return(consulting_nodes)
  
}


# Consult functions -------------------------------------------------------

consult_costs <- function(x, costs_data){
  gp_cost <- costs_data %>% filter(item == "GP consult") %>%
    pull(cost)
  out <- x %>% mutate(consult_cost = gp_cost)
  return(out)
}

consult_tests <- function(x, test_params){
  n <- nrow(x)  #number of patients
  tests_performed <- tibble(id = x$id,
                            test_cost = 0)
  
  #for all tests
  for(i in 1:length(test_params$test)){
    probability <- test_params[i,]$p  #read in probability
    sampled <- sample(c(1,0), size = n, replace = TRUE, prob = c(probability, 1-probability))   #sample whether the test is used
    #create tibble with tests and costs
    tests_performed <- tibble::add_column(tests_performed, sampled) %>%
                        mutate(test_cost = sampled * test_params[i,]$cost + test_cost) %>%
                        rename_with(~as.character(test_params$test[i]), .cols = sampled)
  }
  
  #remove node ids
  tests_performed %<>% select(-id)
  
  #add the performed costs to the careseek tibble
  ret <- x %>%
    tibble::add_column(tests_performed)
  
  return(ret)
}

consult_antibiotics <- function(x, abx_prescribing, abx_types, costs){
  #calculate everything twice, once for nodes aged < 60 and once for nodes aged >= 60
  #except delays, this is the same for both groups
  #there is no difference in abx type that is prescribed
  
  nodes_young <- x %>% filter(age < 60)
  nodes_old <- x %>% filter(age >= 60)

  #calculate number of prescriptions
  p_abx_young <- abx_prescribing %>% 
    filter(agecat == "younger than 60",
           par == "prescriptions") %>%
    pull(p)
  
  p_abx_old <- abx_prescribing %>% 
    filter(agecat == "60 and older",
           par == "prescriptions") %>%
    pull(p)
  
  n_presc_young <- round(p_abx_young * nrow(nodes_young))  
  n_presc_old <- round(p_abx_old * nrow(nodes_old))  
  
  #calculate number of delayed prescriptions
  p_abx_delay <- abx_prescribing %>% 
    filter(par == "prescriptiondelay") %>%
    pull(p)
  
  #get the weights of the different types of antibiotics, for sampling them
  abx_weights <- abx_types %>% pull(p)
  
  #make tibble to sample from
  abx_type <- abx_types %>% 
    select(abx_type = code, abx_days = n_days, abx_delay = delay, abx_cost = cost)
    
  #read in pharmacy fee for abx prescriptions to add to the costs
  pharmacy_fee <- costs %>% filter(item == "pharmacy fee") %>% pull(cost)
  
  # if no pharmacy_fee is provided, set to 0
  if(length(pharmacy_fee) == 0){
    pharmacy_fee <- 0
  }
  
  #sample abx_type data
  #delay is added to a subset of the prescribed antibiotics, probability = p_abx_delay
  prescribed_antibiotics_young <- slice_sample(abx_type, n = n_presc_young, weight_by = abx_weights, replace = TRUE) %>%
    mutate(abx_delay = ifelse(sample(c(1, 0), size = n_presc_young, replace = TRUE, prob = c(p_abx_delay, 1-p_abx_delay)) == 1, abx_delay, 0)) %>%
    add_row(abx_type = "none",
            abx_days = rep(0,nrow(nodes_young)-n_presc_young),
            abx_delay = rep(0,nrow(nodes_young)-n_presc_young),
            abx_cost = rep(0,nrow(nodes_young)-n_presc_young)) %>%
    mutate(abx_cost = abx_cost * abx_days + ifelse(abx_days > 0, pharmacy_fee, 0))
  
  prescribed_antibiotics_old <- slice_sample(abx_type, n = n_presc_old, weight_by = abx_weights, replace = TRUE) %>%
    mutate(abx_delay = ifelse(sample(c(1, 0), size = n_presc_old, replace = TRUE, prob = c(p_abx_delay, 1-p_abx_delay)) == 1, abx_delay, 0)) %>%
    add_row(abx_type = "none",
            abx_days = rep(0,nrow(nodes_old)-n_presc_old),
            abx_delay = rep(0,nrow(nodes_old)-n_presc_old),
            abx_cost = rep(0,nrow(nodes_old)-n_presc_old)) %>%
    mutate(abx_cost = abx_cost * abx_days + ifelse(abx_days > 0, pharmacy_fee, 0))
  
  
  #combine results of young and old
  out_young <- tibble::add_column(nodes_young, prescribed_antibiotics_young)
  out_old <- tibble::add_column(nodes_old, prescribed_antibiotics_old)
  
  return(bind_rows(out_young, out_old))
}




