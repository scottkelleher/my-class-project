
library(foreign)
library(dplyr)
library(tidyr)



test_trend_ca <- function(drug, data = clean_vars){   
  
  
  #load("data/clean_fars.RData")
  
  if(drug %in% c("Stimulant", "Alcohol", "Cannabinoid", "Depressant", "Narcotic", "Other")){
    to_test <- clean_fars%>%
      filter(drug_type == drug) %>%
      group_by(year) %>%
      summarize(positive = sum(positive_for_drug, na.rm = TRUE),
                trials = sum(!is.na(positive_for_drug)))
    ca_alcohol <- prop.trend.test(x = to_test$positive,
                                  n = to_test$trials)
    
    Z <- round(sqrt(as.double(ca_alcohol$statistic)), digits = 2)
    
    p.value <- round((ca_alcohol$p.value), digits = 3)
    
    data.frame(Z, p.value)
  } else{
    drug="Alcohol"
    to_test <- clean_fars%>%
      filter(drug_type != drug) %>%
      group_by(year) %>%
      summarize(positive = sum(positive_for_drug, na.rm = TRUE),
                trials = sum(!is.na(positive_for_drug)))
    ca_alcohol2 <- prop.trend.test(x = to_test$positive,
                                  n = to_test$trials)
    
    Z <- round(sqrt((ca_alcohol2$statistic)), digits =2)
    
    p.value <- round((ca_alcohol2$p.value),digits = 3)
    
    data.frame(Z, p.value)
    
  }
  
}
  
