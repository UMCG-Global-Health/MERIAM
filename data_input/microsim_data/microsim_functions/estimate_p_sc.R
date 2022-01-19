estimate_p_sc <- function(n_cycles, sex_ind = NA, age_ind = NA, ill_cat = NA,  inputdata = NA){
    # median <- 6
    # sd <- (log(10) - log(4))/1.349 #formula: https://stat.ethz.ch/~stahel/lognormal/basics.htm
    # 
    # #survivor function:
    # #The cumulative hazard H(t) = - log(1 - F(t)) is -plnorm(t, r, lower = FALSE, log = TRUE). (lognormal R documentation)
    # #Briggs pages 52-53 for formulas to calculate transition probabilities
    # hf <- -plnorm(0:n_cycles, log(median) , sd, lower.tail = FALSE, log.p = TRUE)
    # 
    # 
    # p_sh <- tibble(t = 0:n_cycles,
    #                hazard = hf,
    #                hazard_prev = c(0, hf[-(n_cycles+1)]),
    #                survival = exp(-hf),
    #                p = 1-exp(hazard_prev-hazard)) %>%
    #   slice(-1)
    # 
    # 
    # p_sh$p
  
  #immediately return results of function above for, to save time (with a max of 50 cycles)
  
    dat <- c(0.004171056, 0.048926725, 0.106489569, 0.143601176, 0.164081858, 0.174661999, 0.179534905, 0.181070050,
              0.180617234, 0.178967598, 0.176598851, 0.173807035, 0.170779464, 0.167636486, 0.164456158, 0.161289261,
              0.158168670, 0.155115340, 0.152142203, 0.149256749, 0.146462762, 0.143761504, 0.141152526, 0.138634233,
              0.136204279, 0.133859841, 0.131597816, 0.129414954, 0.127307955, 0.125273538, 0.123308479, 0.121409647,
              0.119574023, 0.117798709, 0.116080934, 0.114418060, 0.112807578, 0.111247106, 0.109734386, 0.108267281,
              0.106843763, 0.105461919, 0.104119933, 0.102816089, 0.101548765, 0.100316422, 0.099117606, 0.097950938,
              0.096815113, 0.095708892)
    
    return(dat[1:n_cycles])
}
