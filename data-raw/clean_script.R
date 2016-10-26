
library(foreign)
library(dplyr)
library(tidyr)

 





clean_yearly_person_file <- function(study_year){
  
  datafr <- read.dbf(paste0("data-raw/yearly_person_data/PERSON_",study_year, ".dbf"))
  colnames(datafr) <- tolower(colnames(datafr))
  
  
  datafr <- datafr %>%
    select(state, st_case, veh_no, per_no, sex, death_yr, age, drinking, alc_res, drugres1, drugres2, drugres3, lag_hrs, lag_mins, per_typ, inj_sev) %>%
   filter(per_typ == 1 & inj_sev == 4) %>%
    rename(year=death_yr)
  
  
  datafr <- unite(datafr, unique_id, st_case, veh_no, per_no)
  datafr$unique_id <- paste(datafr$unique_id, study_year, sep = "_")
   datafr <- filter(datafr, state %in% c(6, 15, 17, 33, 44, 54)) %>%
     select(-state, -per_typ, -inj_sev)
   
   unique(datafr$sex)
   
   datafr$sex <- as.factor(datafr$sex) 
   levels(datafr$sex)[levels(datafr$sex)=="1"] <- "Male"
   levels(datafr$sex)[levels(datafr$sex)=="2"] <- "Female"
   levels(datafr$sex)[levels(datafr$sex)=="9"] <- NA
   
   #datafr<- filter(datafr, datafr$alc_res > as.numeric(0))
   datafr$alc_res <-ifelse(datafr$alc_res == 95 | datafr$alc_res == 96 | datafr$alc_res == 97 | datafr$alc_res == 98 | datafr$alc_res == 99, NA, datafr$alc_res)
   datafr <- mutate(datafr, Alcohol = ifelse(datafr$alc_res> 0, TRUE, FALSE)) %>% 
     select(-alc_res, -drinking) # %>%
   #filter(Alcohol, (!is.na))
   
   #datafr <- mutate(datafr, one_hour_dead = NA)
   
   datafr$lag_hrs = ifelse(datafr$lag_hrs == 999 | datafr$lag_hrs == 88 | datafr$lag_hrs == 99, NA, datafr$lag_hrs)
   datafr <- filter(datafr, (datafr$lag_hrs*60 + datafr$lag_mins) < 61)
   datafr <- select(datafr, -lag_hrs, -lag_mins)
   
   datafr$age = ifelse(datafr$age == 999 | datafr$age == 99, NA, datafr$age)
   datafr$age <- (cut(datafr$age,  breaks=c(0, 25, 44, 65,98)))
  datafr$age <- as.factor(datafr$age)
  datafr <- rename(datafr, agecat = age)
  
  levels(datafr$agecat)[levels(datafr$agecat)=="(0,25]"] <- "< 25 years"
  levels(datafr$agecat)[levels(datafr$agecat)=="(25,44]"] <- "25--44 years"
  levels(datafr$agecat)[levels(datafr$agecat)=="(44,65]"] <- "44--65 years"
  levels(datafr$agecat)[levels(datafr$agecat)=="(65,98]"] <- "> 65 years"
  
 
  gathered_df <- datafr %>%
    tidyr::gather(drug_number, drug_type_raw, contains("drugres")) %>%
    dplyr::mutate(drug_type = ifelse(drug_type_raw %in% 100:295,
                                     "Narcotic", NA),
                  drug_type = ifelse(drug_type_raw %in% 300:395,
                                     "Depressant", drug_type),
                  drug_type = ifelse(drug_type_raw %in% 400:495,
                                     "Stimulant", drug_type),
                  drug_type = ifelse(drug_type_raw %in% 600:695,
                                     "Cannabinoid", drug_type),
                  drug_type = ifelse(drug_type_raw %in% c(500:595, 700:996),
                                     "Other", drug_type),
                  drug_type = ifelse(drug_type_raw == 1,
                                     "None", drug_type),
                  drug_type = factor(drug_type)) %>%
    dplyr::select(-drug_type_raw, -drug_number) %>%
    # Filter out any observations where both alcohol and drug data is missing
    dplyr::filter(!(is.na(Alcohol) & is.na(drug_type)))
  
  # Create a subset with only individuals with at least one non-missing
  # listing for drugs
  non_missing_drugs <- gathered_df %>%
    filter(!is.na(drug_type)) %>%
    group_by(unique_id, drug_type) %>%
    summarize(has_drug = TRUE) %>%
    ungroup() %>%
    mutate(row_num = 1:n()) %>%
    spread(drug_type, has_drug, fill = FALSE) %>%
    select(-row_num) %>%
    group_by(unique_id) %>%
    summarize(Cannabinoid = any(Cannabinoid),
              Depressant = any(Depressant),
              Narcotic = any(Narcotic),
              Other = any(Other),
              Stimulant = any(Stimulant)) %>%
    ungroup()
  
  
  # Join this back into the full dataset
  datafr <- datafr %>%
    dplyr::select(-contains("drugres")) %>%
    dplyr::full_join(non_missing_drugs, by = "unique_id") %>%
    tidyr::gather(drug_type, positive_for_drug, Alcohol, Cannabinoid,
                  Depressant, Narcotic, Other, Stimulant) %>%
    dplyr::mutate(drug_type = factor(drug_type))
  
 
  
  return(datafr)
}


# Apply the function to clean the data across the study years
for(study_year in 1999:2010){
  print(study_year)
  datafr <- clean_yearly_person_file(study_year)
  if(study_year == 1999){
    clean_fars <- datafr
  } else {
    clean_fars <- rbind(clean_fars, datafr)
  }
}
save(clean_fars, file = "data/clean_fars.RData")




                          