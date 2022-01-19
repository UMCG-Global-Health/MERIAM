calc_amr <- function(data, results, current_year, initialize = FALSE){
  # BSP: broad spectrum penicillin
  # 3G: cephalosporins (3rd gen)
  # C: carbapenem
  # F: quinolones
  # M: macrolides
  # 
  
  if(initialize == FALSE){
    # write_rds(list(data = data, results = results, current_year = current_year), 
    #           str_c("data_output/debug_",as.double(Sys.time()),".RDS"))
    #calculate relative reduction in antibiotic consumption
    ab_red <- data %>%
      left_join(select(results, country, strategy, iteration, microsim_abxcon), 
                by = c("country", "strategy", "iteration")) %>%
      mutate(
        n_ind = map_dbl(nodes,
                        ~.x %>%
                          filter(active == 1) %>%
                          nrow()),
        abx_pred_cons = map2(nodes, abx_con,
                             ~.y %>% 
                               filter(year == current_year - 1) %>%
                               mutate(pred_ddd = mean * 364 ) %>% # 364, as only 364 days are modelled anually (7*52)
                               select(class = code, year, pred_ddd)),
        abx_meas_cons = map2(microsim_abxcon, n_ind,
                             ~.x %>%
                               add_row(tibble(year = current_year - 1, # add a row with consumption set to 0, to avoid errors if there is no AMX consumption
                                              week = 0,
                                              abx  = "AMX",
                                              ddd = 0)) %>%
                               filter(year == current_year - 1) %>%
                               group_by(year, abx) %>%
                               summarise(ddd = sum(ddd), .groups = "drop") %>%
                               mutate(ddd = (ddd / .y) * 1000,
                                      class = case_when(
                                        abx == "AMX" ~ "BSP",
                                        abx == "FLC" ~ "NSP",
                                        abx == "TCY" ~ "T",
                                        abx == "AMC" ~ "other",
                                        abx == "MID" ~ "M",
                                        abx == "OFX" ~ "other",
                                        abx == "LEX" ~ "3GC",
                                        is.character(abx) ~ "other"
                                      ))),
        abx_sum = map2(abx_meas_cons, abx_pred_cons,
                       ~.x %>%
                         filter(class != "other") %>%
                         select(year, class, sim_ddd  = ddd) %>%
                         left_join(.y, by = c("year", "class")))) %>%
      select(country, strategy, iteration, abx_sum, amr) %>%
      unnest(abx_sum)
    
    #for now, only BSP is included in the analyses
    ab_red <- filter(ab_red, class == "BSP")
    
    
    ab_red2 <- ab_red %>%
      pivot_wider(names_from = "strategy", names_prefix = "ddd_", values_from = c("sim_ddd")) %>%
      mutate(ref = ddd_base) %>%
      mutate(across(.cols = starts_with("ddd"),
                    .fns = ~ 1 - .x / ref,
                    .names = "red_{.col}"),
             across(.cols = starts_with("red_ddd_"),
                    .fns = ~.x * (ref / pred_ddd),
                    .names = "total_{.col}")) %>%
      select(country, iteration, class, starts_with("total_red_ddd_"), amr) %>%
      pivot_longer(cols = starts_with("total_red_ddd_"), 
                   names_to = "strategy", 
                   names_prefix = "total_red_ddd_", 
                   values_to = "reduction") %>%
      group_by(country, iteration, strategy, amr) %>%
      nest() %>%
      rename(ab_red = data) %>%
      ungroup()
    
    ab_red2 <- ab_red %>%
      pivot_wider(names_from = "strategy", names_prefix = "ddd_", values_from = c("sim_ddd")) %>%
      mutate(ref = ddd_base) %>%
      mutate(across(.cols = starts_with("ddd"),
                    .fns = ~ ref - .x,
                    .names = "red{col}")) %>%
      pivot_longer(cols = c(starts_with("ddd_"), starts_with("redddd")), 
                   names_to = c(".value", "strategy"),
                   names_pattern = ("(.*)_(.*)")) %>%
      mutate(reduction = redddd / pred_ddd) %>%
      nest(ab_red = c("class", "reduction")) %>%
      select(country,
             iteration,
             amr, 
             strategy,
             ab_red)
    
    #calculate the elasticity, using the minimum consumption and the maximum consumption
    #currenty only PRSP implemented
    #load the model
    model_prsp <- data$amr_elas[1] %>% pluck("PRSP")
    

    #calculate the elasticity: using CRP as the min and base as the max bound for the midpoint method
    elasticity_pre <- ab_red2 %>%
      select(country, iteration, strategy, ab_red) %>%
      #pivot_wider(names_from = "strategy", names_prefix = "ab_red_", values_from = "ab_red") %>%
      mutate(red = map_dbl(ab_red,
                           ~.x %>% filter(class == "BSP") %>%
                             pull(reduction)),
             predicted = map2_dbl(country, iteration,
                                  ~data %>% filter(country == .x, iteration == .y, strategy == "base") %>%
                                    pull(abx_con) %>%
                                    flatten_dfr() %>%
                                    filter(year == current_year - 1,
                                           code == "BSP") %>%
                                    pull(mean))) %>%
      rowwise() %>%
      mutate(con = (1-red) * predicted) %>%
      ungroup() %>%
      select(country, iteration, strategy, con)
    
    elasticity <- elasticity_pre %>%
      pivot_wider(names_from = "strategy", names_prefix = "con_", values_from = "con") %>%
      mutate(min = map2_dbl(country, iteration,
                            ~elasticity_pre %>%
                              filter(country == .x, iteration == .y) %>%
                              pull(con) %>%
                              min()),
             max = map2_dbl(country, iteration,
                            ~elasticity_pre %>%
                              filter(country == .x, iteration == .y) %>%
                              pull(con) %>%
                              max())) %>%
      rowwise() %>%
      mutate(elasticity = calc_elasticity(model_prsp, min, max),
             pair = "PRSP") %>%
      ungroup() %>%
      select(country, iteration, pair, elasticity)
    
    
    
    #calculate what this means for the amr rates and return as a tibble 
    out <- ab_red2 %>%
      ungroup() %>%
      mutate(elasticity = map2(country, iteration,
                               ~elasticity %>%
                                 filter(country == .x,
                                        iteration == .y) %>%
                                 select(pair, elasticity),
                               elasticity),
             amr = pmap(list(amr, ab_red, elasticity),
                        ~..1 %>%
                          left_join(..2, by = c("abx" = "class")) %>%
                          left_join(..3, by = "pair") %>%
                          mutate(data = map(data,
                                            ~.x %>% 
                                              filter(year == current_year) %>%
                                              select(value))) %>%
                          unnest(data) %>%
                          rowwise() %>%
                          mutate(amr_value = value * ifelse(!is.na(reduction)&is.double(reduction)&reduction != 0, 1-reduction*elasticity, 1)) %>%
                          ungroup() %>%
                          select(-geo))) %>%
      select(country, iteration, strategy, amr) %>%
      mutate(year = current_year) 
  } else if(initialize == TRUE){
    #if initalialize == TRUE, only get projected AMR value for starting year
    out <- data %>% 
      select(country, iteration, strategy, amr) %>%
      unnest(amr) %>%
      mutate(amr_value = map_dbl(data,
                        ~.x %>%
                          filter(year == current_year) %>%
                          pull(value)),
             year = current_year) %>%
      select(-geo, -data) %>%
      group_by(country, iteration, strategy, year) %>%
      nest() %>%
      ungroup() %>%
      select(country, iteration, strategy, amr = data, year)
      
  }
    
  return(out)   
}

calc_elasticity <- function(model, min, max){
  low_consumption <- min
  up_consumption <- max

  new_data <- tibble(
    consumption = c(low_consumption, up_consumption),
    amr_level = NA
  )
  
  predictions <- predict(model, new_data)
  
  #percentage change in consumption
  pcc <- (up_consumption - low_consumption) / ((up_consumption + low_consumption)/2)
  
  #percentage change in amr levels
  pca <- (predictions[2] - predictions[1])/((predictions[2] + predictions[1]) / 2)
  
  return(pca/pcc)
}
