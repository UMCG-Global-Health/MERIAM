# functions used to calculate CAP burden
# 


hosp_cap <- function(data, results, amr, current_year){
  #create a list with the different age categories
  age_cats <- list(
    c(18, 49),
    c(50,64),
    c(65, 74),
    c(75, 84),
    c(85, 120)
  )
  
  #sample nodes with cap
  nodes_cap <- future_pmap(list(data$nodes, data$cap, data$longterm),
                        function(nodes, cap_data, longterm){
                         
                          nodes_hosp <- foreach (a=rep(1:length(age_cats)),
                                                .combine = "rbind",
                                                .inorder = FALSE) %do% {
                                                  
                                                  selected_nodes <- nodes %>%
                                                    filter(age >= age_cats[[a]][1],
                                                           age <= age_cats[[a]][2],
                                                           active == 1) %>%
                                                    mutate(causative_agent = NA,
                                                           mort = NA)
                                                  
                                                  n_nodes <- nrow(selected_nodes)
                                                  
                                                  
                                                  incidence_inpatient <- cap_data$incidence %>%
                                                    filter(min_age == age_cats[[a]][1],
                                                           max_age == age_cats[[a]][2],
                                                           setting == "inpatient",
                                                           item == "incidence",
                                                           disease == "cap") %>%
                                                    pull(mean)
                                              
                                                  n_inpatient <- round((incidence_inpatient / 100000) * n_nodes)
                                                  
                                                  if(n_inpatient > 0){
                                                    cause <- sample(c("sp", "other"), size = n_inpatient, replace = TRUE, prob = c(cap_data$agents["sp"], 1 - cap_data$agents["sp"]))
                                                    
                                                    hospitalized <- selected_nodes %>%
                                                      slice_sample(n = n_inpatient) %>%
                                                      mutate(causative_agent = cause)
                                                    
                                                    
                                                    #if within age thresholds, apply utility decrement
                                                    if(age_cats[[a]][1] >= longterm$utility_decrement$cap$start_age & age_cats[[a]][2] <= longterm$utility_decrement$cap$end_age){
                                                      hospitalized <- hospitalized %>%
                                                        mutate(uti_dec = uti_dec + longterm$utility_decrement$cap$value)
                                                    }
                                                    
                                                    #add mortality
                                                    cfr_cap <- cap_data$incidence %>%
                                                    filter(min_age == age_cats[[a]][1],
                                                           max_age == age_cats[[a]][2],
                                                           setting == "inpatient",
                                                           item == "cfr",
                                                           disease == "cap") %>%
                                                      pull(mean)
                                                    
                                                    cfr_sp <- cap_data$incidence %>%
                                                      filter(min_age == age_cats[[a]][1],
                                                             max_age == age_cats[[a]][2],
                                                             setting == "inpatient",
                                                             item == "cfr",
                                                             disease == "ipd") %>%
                                                      pull(mean)
                                                    
                                                    
                                                    hospitalized_cap <- hospitalized %>%
                                                      filter(causative_agent == "other")
                                                    
                                                    mort_cap <- sample(c(T,F), size = nrow(hospitalized_cap), replace = TRUE, prob = c(cfr_cap, 1-cfr_cap))
                                                    
                                                    hospitalized_sp <- hospitalized %>%
                                                      filter(causative_agent == "sp")
                                                    
                                                    mort_sp <- sample(c(T,F), size = nrow(hospitalized_sp), replace = TRUE, prob = c(cfr_sp, 1-cfr_sp))
                                                    
                                                    output <- bind_rows(hospitalized_cap, hospitalized_sp) %>%
                                                      mutate(mort = c(mort_cap, mort_sp))
                                                    
                                                  }else(
                                                    output <- selected_nodes %>%
                                                      slice(n = 0)
                                                  )
                                                  
                                                  return(output)
                                                  
                                                }
                          
                        mort_ids <- nodes_hosp %>%
                          filter(mort == T) %>%
                          pull(id)

                        
                        nodes_out <- nodes_hosp %>%
                          select(-mort, -causative_agent) %>%
                          bind_rows(nodes %>%
                                      filter(!(id %in% nodes_hosp$id))) %>%
                          mutate(active = ifelse(id %in% mort_out, 0, 1))
                        
                       
                        
                          
                        }, .options = furrr_options(seed = TRUE))
  
  return(incidence_cap)
}

# function to calculate incidence from eurostat object
# incidence <- foreach (a=rep(1:length(age_cats), times = 2), 
#                       s = rep(c("M","F"), each = length(age_cats)),
#                       .combine = "rbind",
#                       .inorder = FALSE) %do% {
#                         
#                         selected_nodes <- nodes %>%
#                           filter(age >= age_cats[[a]][1],
#                                  age <= age_cats[[a]][2],
#                                  sex == ifelse(s == "M", 0, 1),
#                                  active == 1) %>%
#                           mutate(icd = "J12-J18")
#                         
#                         n_nodes <- nrow(selected_nodes)
#                         
#                         
#                         selected_incidence <- admissions %>%
#                           filter(min_age == age_cats[[a]][1],
#                                  max_age == age_cats[[a]][2],
#                                  sex == s,
#                                  icd10 == "J12-J18") %>%
#                           pull(value)
#                         
#                         n_hosp <- round((selected_incidence / 100000) * n_nodes)
#                         
#                         if(n_hosp > 0){
#                           output <- selected_nodes %>%
#                             slice_sample(n = n_hosp)
#                           
#                           #if within age thresholds, apply utility decrement
#                           if(age_cats[[a]][1] >= longterm$utility_decrement$cap$start_age & age_cats[[a]][2] <= longterm$utility_decrement$cap$end_age){
#                             output <- output %>%
#                               mutate(uti_dec = uti_dec + longterm$utility_decrement$cap$value)
#                           }
#                           
#                         }else(
#                           output <- selected_nodes %>%
#                             slice(n = 0)
#                         )
#                         
#                         return(output)
#                         
#                       }