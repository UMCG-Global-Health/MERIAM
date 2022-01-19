# old function based on Mangen et al. 2017

# estimate_p_hc <- function(n_cycles){
#     median <- 7
#     sd <- (log(11) - log(5))/1.349 #formula: https://stat.ethz.ch/~stahel/lognormal/basics.htm
# 
#     #survivor function:
#     #The cumulative hazard H(t) = - log(1 - F(t)) is -plnorm(t, r, lower = FALSE, log = TRUE). (lognormal R documentation)
#     #Briggs pages 52-53 for formulas to calculate transition probabilities
#     hf <- -plnorm(0:n_cycles, log(median) , sd, lower.tail = FALSE, log.p = TRUE)
# 
# 
#     p_hc <- tibble(t = 0:n_cycles,
#                    hazard = hf,
#                    hazard_prev = c(0, hf[-(n_cycles+1)]),
#                    survival = exp(-hf),
#                    p = 1-exp(hazard_prev-hazard)) %>%
#       slice(-1)
# 
# 
#     p_hc$p
# }



# new function based on eurostat data


estimate_pc_hc <- function(n_cycles, sex_ind, age_ind, ill_cat,  inputdata){

  # av_los <- inputdata %>%
  #   filter(min_age <= age_ind,
  #          max_age >= age_ind,
  #          sex == sex_ind,
  #          hosp_reason == ill_cat)  %>%
  #   pull(av_los)
  
  #read in matrix, where the first 100 rows are for men (sex_ind == 0) and the second 100 rows for women (sex_ind ==1)
  inputdata[(sex_ind*100 + age_ind+1), ill_cat]

  
}
