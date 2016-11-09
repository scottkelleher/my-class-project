
library(foreign)
library(dplyr)
library(tidyr)



perc_cis <- function(x, n){   
  
  #load("data/clean_fars.RData")
  # 
  # all_deaths <- filter(clean_fars, !is.na(positive_for_drug))
  # drug_users <- filter(clean_fars, positive_for_drug == TRUE)
  # x <- length(drug_users$unique_id)
  # n <- length(all_deaths$unique_id)
  # 
  p <- round(x/n, digits = 3)*100
  l <- x/n
  
  se <- sqrt((l*(1-l))/n)
  
  to <- round(l + (1.96 * se), digits = 3)*100 
  jo <- round(l - (1.96 * se), digits = 3)*100 
   
  lower_upper <- c(jo ,to)
 
  paste0(p,"%", " (", jo,"%", ",", to,"%", ")")
  
} 
  
  
  
  
  
  
  
  
  
  
  #length(clean_fars$drug_type == "Alcohol" & clean_fars$positive_for_drug == TRUE)
  