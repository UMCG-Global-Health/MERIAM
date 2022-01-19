####functions to model antibiotics prescribing
#function to set number of days the prescribed antibiotic is taken
init_abx_days <- function(x, abx_days){
  #return the number of days of the delay
  switch(x,
         "amo" = abx_days["amo"],
         "dox" = abx_days["dox"],
         "none" = as.numeric(0))
}

#function to sample durates of antibiotic prescription delay
init_abx_delay <- function(x, p_abx_delay, abx_delay_duration){
  delay <- as.numeric(0)
  abx <- str_detect(x, "none", negate = TRUE) #return TRUE in case of antibiotic prescription
  
  if(abx){
    delay <- sample(c(abx_delay_duration,0), size = 1, replace = TRUE, prob = c(p_abx_delay, (1 - p_abx_delay))) #sample the probability of delayed prescription
  }
  
  return(delay)
}


