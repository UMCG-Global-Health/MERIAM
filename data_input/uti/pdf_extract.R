library(checkpoint)
checkpoint("2020-11-20", checkpointLocation = getwd(), scanForPackages = FALSE)


library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(furrr)
library(forcats)
library(stringr)
library(readxl)
library(readr)
library(lubridate)
library(ggplot2)
library(tabulizer)

countries <- c("be", "dk", "gb-england", "fi", "fr", "de", "gr", "hu", "it", "nl", "si", "es", "se", "gb", "am")
pages <- c(57, 67, 73, 79, 80, 89, 95, 100, 105, 115, 125, 130, 136, 146, 157)
sets <- c("EUVAS", "TTO", "VAS")

data <- tibble(sex = character(),
               mean = double(),
               se = double(),
               `25perc` = double(),
               `50perc` = double(),
               `75perc` =  double(),
               minage = double(),
               maxage = double(),
               geo = character(),
               value_set = character())

for(i in 1:length(countries)) {
  for(j in 1:3){
    print(paste0("For '", countries[i],"' select: ",sets[j]))
    pdf <- extract_areas("data_input/uti/10.1007_978-94-007-7596-1.pdf",
                         pages = pages[i] + 15 + ifelse(j == 3, 1, 0))
    
    set <- readline(prompt = "Enter EUVAS, TTO, VAS or NONE:  ")
    
    if(set != "NONE"){
      data_current <- pdf %>%
        as.data.frame(row.names = c("total_mean",
                                    "total_se",
                                    "total_25perc",
                                    "total_50perc",
                                    "total_75perc",
                                    "male_mean",
                                    "male_se",
                                    "male_25perc",
                                    "male_50perc",
                                    "male_75perc",
                                    "female_mean",
                                    "female_se",
                                    "female_25perc",
                                    "female_50perc",
                                    "female_75perc")) %>%
        as_tibble(rownames = NA) %>%
        rownames_to_column(var = "set") %>%
        pivot_longer(cols = 2:9,
                     names_to = "agecat",
                     values_to = "value") %>%
        mutate(sex = str_extract(set, "[:alpha:]+(?=_)"),
               indicator = str_extract(set, "(?<=_)[:alnum:]+")) %>%
        select(-set) %>%
        pivot_wider(names_from = "indicator",
                    values_from = "value") %>%
        filter(agecat != "X8") %>%
        mutate(minage = case_when(agecat == "X1" ~ 18,
                                  agecat == "X2" ~ 25,
                                  agecat == "X3" ~ 35,
                                  agecat == "X4" ~ 45,
                                  agecat == "X5" ~ 55,
                                  agecat == "X6" ~ 65,
                                  agecat == "X7" ~ 75),
               maxage = case_when(agecat == "X1" ~ 24,
                                  agecat == "X2" ~ 34,
                                  agecat == "X3" ~ 44,
                                  agecat == "X4" ~ 54,
                                  agecat == "X5" ~ 64,
                                  agecat == "X6" ~ 74,
                                  agecat == "X7" ~ 99),
               across(.cols = 3:9, .fns = as.double),
               geo = countries[i],
               value_set = set) %>%
        select(-agecat)
      
      data <- add_row(data, data_current)
      print(data_current)
    }
    
  }
}

data <- data %>% relocate(geo, value_set)

saveRDS(data, "data_input/uti/base_utilities.RDS")


