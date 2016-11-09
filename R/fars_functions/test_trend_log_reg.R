
library(foreign)
library(dplyr)
library(tidyr)


test_trend_log_reg <- function(drug, data = clean_vars){ 




  if(drug %in% c("Stimulant", "Alcohol", "Cannabinoid", "Depressant", "Narcotic", "Other")){

to_test <- clean_fars %>%
  filter(drug_type == drug)
log_reg <- glm(positive_for_drug ~ year, data = to_test,
               family = binomial(link = "logit"))
jj <- summary(log_reg)$coefficients
Z= round(jj[2,3], digits =2)
p.value = round(jj[2,4], digits = 3)
data.frame(Z, p.value)

}else{
  
  to_test <- clean_fars %>%
    filter(drug_type != "Alcohol")
  log_reg <- glm(positive_for_drug ~ year, data = to_test,
                 family = binomial(link = "logit"))
  summary(log_reg)$coefficients
  jj <- summary(log_reg)$coefficients
  Z= round(jj[2,3], digits = 2)
  p.value = round(jj[2,4], digits = 3)
  data.frame(Z, p.value)
}  

}    