
library(foreign)
library(dplyr)
library(tidyr)
study_year = 1999

clean_yearly_person_file <- function(datafr = study_year){
  
  datafr <- read.dbf(paste0("data-raw/yearly_person_data/PERSON_",year, ".dbf"))
  colnames(datafr) <- tolower(colnames(datafr))
  
  
  datafr <- datafr %>%
    select(state, st_case, death_yr, veh_no, per_no, sex, age, drinking, alc_res, drugs, drugres1, drugres2, drugres3, drugs, lag_hrs, lag_mins, per_typ, inj_sev) %>%
   filter(per_typ == 1 && inj_sev == 4) 
  
  
  datafr <- unite(datafr, unique_id, st_case, veh_no, per_no)
  datafr$unique_id <- paste(datafr$unique_id, study_year, sep = "_")
   datafr <- filter(datafr, state %in% c(6, 15, 17, 33, 44, 54)) %>%
     select(-state)
   
   unique(datafr$sex)
   
   datafr$sex <- as.factor(datafr$sex) 
   levels(datafr$sex)[levels(datafr$sex)=="1"] <- "Male"
   levels(datafr$sex)[levels(datafr$sex)=="2"] <- "Female"
   levels(datafr$sex)[levels(datafr$sex)=="9"] <- NA
   
   #datafr<- filter(datafr, datafr$alc_res > as.numeric(0))
   datafr <- mutate(datafr, Alcohol = ifelse(datafr$alc_res> 0, TRUE, FALSE)) %>%   ###### need to make any empty values here be NA
     select(-alc_res, -drinking)
   
   #datafr <- mutate(datafr, one_hour_dead = NA)
   
   datafr$lag_hrs = ifelse(datafr$lag_hrs == 999 | datafr$lag_hrs == 88 | datafr$lag_hrs == 99, NA, datafr$lag_hrs)
   datafr <- filter(datafr, (datafr$lag_hrs*60 + datafr$lag_mins) < 61)
   datafr <- select(datafr, -lag_hrs, -lag_mins)
   
   datafr$age = ifelse(datafr$age == 999 | datafr$age == 99, NA, datafr$age)
   
   
  # if(((datafr$lag_hrs*60) + datafr$lag_mins) < 61 ){
   #  datafr$one_hour_dead = 1
   #}  else {
  #  datafr$one_hour_dead <- 0
  # }

   
   
  return(datafr)
}

class

        
                          