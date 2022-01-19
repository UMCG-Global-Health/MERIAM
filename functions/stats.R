####general functions

#proportion
calc_proportion <- function(p, n, mode = "p"){
  
  if(n < 20){warning("n is too small for normal approximation")}
  if(p < 5){warning("p is too small for normal approximation")}
  if(n-5 < 5){warning("p is too large for normal approximation")}
  
  prop <- p/n
  
  if(mode == "p"){return(prop)}
  
  sd <- sqrt((prop*(1 - prop))/n)
  if(mode == "sd"){return(sd)}

  cilow <- prop - 1.96*sd
  cihigh <- prop + 1.96*sd
  
  if(mode == "ci"){
    return(c(cilow, cihigh))
  } else if(mode == "all"){
    out <- c(prop, sd, cilow, cihigh)
    names(out) <- c("prop", "sd", "cilow", "cihigh")
    return(out)
  } else{
    warning("incorrect mode")
    break
  }
  
  
}

rprop <- function(n, mean, sd){
  #from Briggs page 89
  alpha <- mean * (((mean*(1-mean))/(sd^2))-1)
  beta <- alpha * ((1 - mean)/mean)
  
  rbeta(n, alpha, beta)
}


### convert consumer prices
# is using data generated from eurostat

convert_price <- function(x, 
                          country = NA, 
                          year_in = NA,
                          year_out = NA, 
                          input = c("local", "EUR"),
                          output = c("ppp", "EUR"),
                          eurostat_data = list(cpi = read.csv("data_input/financial/cpi.csv"),
                                               ppp = read.csv("data_input/financial/ppp.csv"),
                                               exchange_rate = read.csv("data_input/financial/exchange_rates.csv")),
                          conversion_factor = FALSE){
  if(length(eurostat_data) != 3){
    print("Check whether eurostat_data is correct")
  }
  
  
  data_cpi <- eurostat_data$cpi
  data_ppp <- eurostat_data$ppp
  data_exchangerate <- eurostat_data$exchange_rate %>%
    add_row(tibble(currency = "EUR", year = year_out, values = 1))
  
  #Helper function
  
  # fct_ppp <- function(country, year_out, ppp_country){
  #   ret <- data_ppp %>% filter(geo == country, year == year_out) %>% pull(value)
  #   
  #   if(ppp_country == "USD"){
  #     fct_usd <- data_ppp %>% filter(geo == "us", year == year_out) %>% pull(value)
  #     ret <- ret / fct_usd
  #   }
  #   
  #   return(ret)
  # }
  
  # Check whether x and year_in are of the same length to prevent errors
  # 
  if(length(x) != length(year_in) && length(year_in) > 1){
    return(warning("x and year_in should be of the same length, or year_in should be of length 1"))
  }
  
  # if year_in is an integer, convert to double
  
  if(is_integer(year_in)){
    year_in <- as.double(year_in)
  }
  
  inflation <- 1
  
  # check if data for input year is available
  
  #only keep the years that are available in all three datasets
  de_duplicated1 <- c(levels(as.factor(eurostat_data$cpi$year)), levels(as.factor(eurostat_data$ppp$year)))[duplicated(c(levels(as.factor(eurostat_data$cpi$year)), levels(as.factor(eurostat_data$ppp$year))))]
  avail_years <- c(de_duplicated1, levels(as.factor(eurostat_data$exchange_rate$year)))[duplicated(c(de_duplicated1, levels(as.factor(eurostat_data$exchange_rate$year))))]
  
  year_in <- map_dbl(year_in, function(x, avail_years){
    
    if(is.na(x)){
      out <- NA
    } else if(!(x %in% avail_years)){
      out <- as.double(max(avail_years))
      warning(str_c("The year ", x, " is not available in the dataset - using the year ", out, " instead."))
    } else{
      out <- x
    }
    
    return(out)
  }, avail_years)
  
  # check if data for output year is available
  
  if(!(year_out %in% avail_years)){
    return(warning(str_c("The requested output year ", year_out, " is not available in the dataset.")))
  }
  
  if(is_double(year_out) && is_double(year_in)){
    #correct for inflation
    inflation <- pmap_dbl(list(x = x, year_in = year_in, country = country),
                          function(x, year_in, country, year_out) {
                            if(is.na(x)){
                              return(NA)
                            }
                            level_in <- data_cpi %>% filter(geo == str_to_lower(country), year == year_in) %>% pull(values)
                            level_out <- data_cpi %>% filter(geo == str_to_lower(country), year == year_out) %>% pull(values)
        
                            #inflation:
                            level_out/level_in
    }, year_out = year_out)
  } else if(is.na(year_out) || is.na(year_in)){
    return(warning("Have you forgotten to specify the input or output YEAR???"))
  } else if(length(year_out) > 1){
    return(warning("Please provide only 1 output year"))
  } 
  
  
  
  # conversion factor for USD ppp (compared to EUR ppp)
  fct_usd <- data_ppp %>% filter(geo == "us", year == year_out) %>% pull(value)
  
  # conversion factor for EUR ppp
  fct_ppp <- data_ppp %>% filter(geo == str_to_lower(country), year == year_out) %>% pull(value)
  
  # read in local currency
  local_curr <- data_ppp %>% filter(geo == str_to_lower(country), year == year_out) %>% pull(curr)
  
  # currency exchange rates
  ex_local <- data_exchangerate %>% 
    filter(year == year_out, currency == local_curr) %>%
    pull(values)
  
  ex_input <- data_exchangerate %>% 
    filter(year == year_out, currency == input[2]) %>%
    pull(values)
  
  ex_output <- data_exchangerate %>% 
    filter(year == year_out, currency == output[2]) %>%
    pull(values)
  
  if(conversion_factor == FALSE){
    out <- map2_dbl(x, country,
                    function(x, country, year_out, input, output) {
                      
                      #If cost = NA, return NA
                      
                      if(is.na(x)){
                        return(NA)
                      }
                      
                      # convert x to ppp EUR
                      
                      if(str_to_lower(input[1]) == "ppp"){
                        #if x is already provided in ppp EUR, do nothing
                        convert_input <- x
                        
                        if(str_to_upper(input[2]) == "USD"){
                          #if x is provided in PPP USD, change to ppp EUR
                          convert_input <- convert_input / fct_usd
                        }
                        
                      } else if(str_to_lower(input[1] == "local")){
                        if(str_to_upper(input[2]) == local_curr){
                          #if x is provided in local currency, convert to ppp EUR
                          convert_input <- x / fct_ppp
                        } else{
                          local_curr <- x * (ex_input / ex_local)
                          
                          convert_input <- local_curr / fct_ppp
                        }
                      }
                      
                      
                      # calculate output
                      
                      if(str_to_lower(output[1]) == "ppp" & str_to_upper(output[2]) == "EUR"){
                        #if the output should be ppp EUR, return as is
                        out <- convert_input
                        
                      } else if(str_to_lower(output[1]) == "ppp" & str_to_upper(output[2]) == "USD"){
                        #if output should be ppp USD, convert to USD
                        out <- convert_input * fct_usd
                        
                      } else if(str_to_lower(output[1]) == "local"){
                        #if output should be local currency, convert back to local currency
                        out <- convert_input * fct_ppp
                        
                        #then, if the returned currency should be different from the
                        #local currency used in the country, convert the currency
                        if(str_to_upper(output[2]) != local_curr){
                          out <- out * (ex_output / ex_local)
                        }
                      }
                      
                      #finally, inflate prices based on the cpi
                      
                      return(out)
                      
                      
                    }, year_out = year_out, input = input, output = output)
    
    
    return(out * inflation)
  } else{
    #warnings when too many or too little arguments are entered
    if(x != 1){return(warning("Please set x to 1"))}
    if(length(country) != 1){return(warning("Please provide 1 country when conversion_factor == TRUE"))}
    if(length(year_in) != 1 | length(year_out) != 1){return(warning("Please provide 1 input and output year when conversion factor == TRUE"))}
    
    # convert x to ppp EUR
    
    if(str_to_lower(input[1]) == "ppp"){
      #if x is already provided in ppp EUR, do nothing
      convert_input <- x
      
      if(str_to_upper(input[2]) == "USD"){
        #if x is provided in PPP USD, change to ppp EUR
        convert_input <- convert_input / fct_usd
      }
      
    } else if(str_to_lower(input[1] == "local")){
      if(str_to_upper(input[2]) == local_curr){
        #if x is provided in local currency, convert to ppp EUR
        convert_input <- x / fct_ppp
      } else{
        local_curr <- x * (ex_input / ex_local)
        
        convert_input <- local_curr / fct_ppp
      }
    }
    
    if(str_to_lower(output[1]) == "ppp" & str_to_upper(output[2]) == "EUR"){
      #if the output should be ppp EUR, return as is
      out <- convert_input
      
    } else if(str_to_lower(output[1]) == "ppp" & str_to_upper(output[2]) == "USD"){
      #if output should be ppp USD, convert to USD
      out <- convert_input * fct_usd
      
    } else if(str_to_lower(output[1]) == "local"){
      #if output should be local currency, convert back to local currency
      out <- convert_input * fct_ppp
      
      #then, if the returned currency should be different from the
      #local currency used in the country, convert the currency
      if(str_to_upper(output[2]) != local_curr){
        out <- out * (ex_output / ex_local)
      }
    }
    
    return(out * inflation)
  }
    
    
}

# ### create currency data type
# 
# as_currency <- function(x, currency, year){
#   attr(x, "currency") <- currency
#   attr(x, "currency_year") <- year
#   return(x)
# }



###samplev function

# This code forms the basis for the microsimulation model of the article: 
#
# Krijkamp EM, Alarid-Escudero F, Enns EA, Jalal HJ, Hunink MGM, Pechlivanoglou P. 
# Microsimulation modeling for health decision sciences using R: A tutorial. 
# Med Decis Making. 2018;38(3):400–22.
#
# Please cite the article when using this code
#
# See GitHub for more information or code updates
# https://github.com/DARTH-git/Microsimulation-tutorial
# 
#
# To program this tutorial we made use of 
# R: 3.3.0 GUI 1.68 Mavericks build (7202)
# RStudio: Version 1.0.136 2009-2016 RStudio, Inc.

samplev <- function (probs, m) {
  d <- dim(probs)
  n <- d[1]
  k <- d[2]
  lev <- dimnames(probs)[[2]]
  if (!length(lev)) 
    lev <- 1:k
  ran <- matrix(lev[1], ncol = m, nrow = n)
  U <- t(probs)
  for(i in 2:k) {
    U[i, ] <- U[i, ] + U[i - 1, ]
  }
  if (any((U[k, ] - 1) > 1e-05))
    stop("error in multinom: probabilities do not sum to 1")
  
  for (j in 1:m) {
    un <- rep(runif(n), rep(k, n))
    ran[, j] <- lev[1 + colSums(un > U)]
  }
  ran
}
