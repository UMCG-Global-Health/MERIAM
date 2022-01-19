##### FUNCTIONS TO CALCULATE PRODUCTIVITY LOSSES #####

### Productivity losses

#friction period = 365 / (annual filled vacancies / open vacancies) + 4 weeks (kostenhandleiding: https://tools.ispor.org/PEguidelines/source/NL-Costing_Manual.pdf)

#difficult to estimate outside of the Netherlands, see e.g. https://onlinelibrary.wiley.com/doi/epdf/10.1002/hec.3513

#assume friction period of 12 weeks (see also kostenhandleiding)

calc_prod_individual <- function(days_lost, input_data, method = "friction", empl = "employed"){
  
  if(empl == "unpaid"){
    #assumption: unpaid work is valued at the level of minimum wages
    prod_loss <- days_lost * input_data$wages_minimum_hourly * input_data$minimum_daily_nhours_unemployed
  } else if(empl == "retired"){
    prod_loss <- days_lost * input_data$wages_minimum_hourly * input_data$minimum_daily_nhours_retired
  } else if(empl == "employed"){
    prod_loss <- days_lost * input_data$labour_cost_daily
  } else{
    prod_loss <- 0
  }

  return(prod_loss)
  
}

calc_prod_parent <- function(days_lost, input_data, method = "friction", empl = "employed"){
  
  if(empl %in% c("none", "school")){
    #assumption: unpaid work is valued at the level of minimum wages
    prod_loss <- days_lost * input_data$labour_cost_daily * input_data$labour_loss_parent_factor
  } else{
    prod_loss <- 0
  }
  
  return(prod_loss)
  
}

calc_prod_hc <- function(data, sim_res, year, w, weeks, season = TRUE){
  ndays <- length(weeks) * 7
  pmap(list(data$nodes,
            data$labour,
            sim_res$aggregated),
       function(nodes, productivity_data, ms_aggr, year, w, ndays, season){
         if(productivity_data$productivity_method %in% c("friction", "none")){
           #if friction cost method is used, don't do anything and return an (almost) empty tibble
           tibble(year = year,
                  week = w,
                  product = NA)
         }else{
           #import productivity losses from microsim, if not off season
           if(season){
             ms_prodloss <- ms_aggr %>%
               pull(costs_prod)
           } else{
             ms_prodloss <- 0
             
             #if it is off season, add 1.25 day, to make sure every year has (on average) 365.25 days
             ndays <- ndays + 1.25
           }
           
           #if hc method is used, calculate individual production for all alive nodes
           #then sum it up
           #then, subtract the productivity losses from the microsim
           product <- nodes %>%
             filter(active == 1) %>%
             mutate(productivity = case_when(empl == "employed" ~ calc_prod_individual(ndays, productivity_data, "hc", "employed"),
                                             empl == "retired" ~ calc_prod_individual(ndays, productivity_data, "hc", "retired"),
                                             empl == "unemployed" ~ calc_prod_individual(ndays, productivity_data, "hc", "unemployed"),
                                             is.factor(empl) ~ 0),
                    year = year,
                    week = w) %>%
             group_by(year, week) %>%
             summarise(product = sum(productivity),
                       .groups = "drop") %>%
             mutate(product = product - ms_prodloss)

           
           #return a new row with the results
           return(product)
         }
       }, year, w, ndays, season)
}




### productivity losses for after microsimulation
calc_prod_loss <- function(end_state, 
                           days_lost, 
                           age, 
                           empl, 
                           method = "friction", 
                           productivity_data){
  
  #if no productivity losses are considered, simply return 0
  if(method == "none"){
    return(0)
  }

  res <- mapply(
    function(end_state, days_lost, age, empl, method, productivity_data){
      if(end_state != "death"){
        calc_prod_individual(days_lost, productivity_data, method = method, empl = empl) +
        calc_prod_parent(days_lost, productivity_data, method = method, empl = empl)
      }else if(end_state == "death"){
        if(method == "friction"){
          #productivity losses for friction period are counted (times 7: convert from weeks to days)
          ret_death <- calc_prod_individual(productivity_data$friction_period*7, productivity_data, method == "friction", empl = empl)
        } else{
          #if the human capital approach is used, only calculate losses within the 28-day period
          ret_death <- calc_prod_individual(days_lost, productivity_data, method == "hc", empl = empl)
        }
        
        ret_death + calc_prod_parent(days_lost, productivity_data, method = method, empl = empl)
      }
  }, end_state, days_lost, age, empl, MoreArgs = list(method, productivity_data))
  return(res)
}

### sample employment
calc_employment <- function(data_in, employment_data, output = "node_list"){
  empl_values <- c("none", "school", "employed", "unemployed", "retired")
  age_cats <- c("0-4","5-14", "15-24", "25-54", "55-64", "65-69", "70-74", "75-99")

  if(tibble::is_tibble(data_in)){
    nodes <- data_in %>% filter(active == 1)
    nodes_inactive <- data_in %>% filter(active == 0)
  }else{
    # if the data input is not a tibble, create a tibble
    #data_in should be an array of ages
    nodes <- tibble(age = data_in)
  }
  
  out <- foreach (a= rep(1:length(age_cats)), .combine = "rbind") %dorng% {
    low_thr <- switch(a, 
                     0,
                     5,
                     15,
                     25,
                     55,
                     65,
                     70,
                     75)
    
    high_thr <- switch(a,
                       5,
                       15,
                       25,
                       55,
                       65,
                       70,
                       75,
                       100)
    
    node_cat <- nodes %>%
    filter(age >= low_thr,
           age < high_thr)
    
    if(a == 1){
      #non-school going childen (0-4)
      prob_cats <- c(1,0,0,0,0)
    } else if(a == 2){
      #school going children (all children are assumed to go) (5-14)
      prob_cats <- c(0,1,0,0,0)
    } else if(a == 3){
      #student-age partly employed, partly school-going (15-24)
      p_school <- employment_data %>%
        filter(age == age_cats[a],
               type == "education")%>%
        pull(values)
      
      p_unemployment <- employment_data %>%
        filter(age == age_cats[a],
               type == "unemployment")%>%
        pull(values)
      
      prob_cats <- c(0,p_school,(1-p_school-p_unemployment),p_unemployment,0)
    } else if(a == 4){
      # working age (25-54)
      
      p_unemployment <- employment_data %>%
        filter(age == age_cats[a],
               type == "unemployment")%>%
        pull(values)
      prob_cats <- c(0,0,(1-p_unemployment),p_unemployment,0)
      
    } else if(a == 8){
      # all retired (75-99)
      prob_cats <- c(0,0,0,0,1)
      
    } else {
      # partly retired (55-64) (65-69) (70-74)
      p_retired <- employment_data %>%
        filter(age == age_cats[a],
               type == "pension")%>%
        pull(values)
      
      p_unemployment <- employment_data %>%
        filter(age == "55-74",
               type == "unemployment")%>%
        pull(values)
      
      #in some cases the unemployment and retirement probabilities are more than 1. Then assume that the people that are unemployed have all retired
      if(p_unemployment + p_retired <= 1){
        prob_cats <- c(0,0,(1-p_unemployment-p_retired),p_unemployment,p_retired)
      }else{
        prob_cats <- c(0,0,(1-p_retired),0,p_retired)
      }
      
    }
    
    status <- sample(as.factor(empl_values), 
                     size = nrow(node_cat), 
                     replace = TRUE,
                     prob = prob_cats)
    
    node_cat %>%
      mutate(empl = status)

  }
  
  if(output == "node_list"){
    #return tibble with inactive nodes as well
    return(bind_rows(out, nodes_inactive))
  } else{
    return(out$empl)
  }
  
}



