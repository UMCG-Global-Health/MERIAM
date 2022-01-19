###Functions to incorporate ppas data


#function to incorporate proportion of care-seeking behaviour of 0-4 year olds

ppas_child <- function(probabilistic = FALSE, n = 1){
  if(probabilistic == FALSE){
    child_multiply <- c(0.03050085, 0.008921008) #values are prop_kids and prop_other in ppas data, age.careseek.R
  }else if(probabilistic == TRUE) {
    child_multiply_1 <- rtruncnorm(n, a = 0.0001, mean = 0.03050085, sd = 0.002447114)
    child_multiply_2 <- rtruncnorm(n, a = 0.0001, mean = 0.008921008, sd = 0.001338091)
    child_multiply <- matrix(data = c(child_multiply_1, child_multiply_2), nrow = n, ncol = 2)
  }
  
  return(child_multiply)

}

ppas_abx_type <- function(countrycode){
  #function to load dataset of prescribed antibiotics and convert it to probabilities
  dataset <- read_csv("data_input/generated/data_nl/abx_type-nl.csv", 
                          col_types = cols(X1 = col_skip()))
  
  

  
  total <- dataset[1,2] + dataset[2,2]
  
  sprob <- sapply(dataset$count, proportion, n = total)
  sprob <- t(sprob)
  
  dataset$prop <- unlist(sprob[,1])
  dataset$sd <- unlist(sprob[,2])
  dataset <-as.data.frame(dataset)
  rownames(dataset) <- dataset[,1]
  out <- dataset[,2:4]
  
  return(out)
}
