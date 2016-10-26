library(tidyverse)
library(knitr)
library(ggthemes)
library(foreign)
library(dplyr)
#function to clean each year's data
clean_yearly_person_file <- function(which_year) {
  new_file_name <- paste0("./data-raw/yearly_person_data/person ", 
                          which_year,".dbf")
  # conditions for drugs test
  no_drugs <- 1
  narcotic <- 100:295
  depress <- 300:395
  stimulant <- 400:495
  cannabin <- 600:695
  other_drug <- c(500:595, 700:795, 800:895, 900:995, 996)
  drug_cond <- c(no_drugs, narcotic, depress, stimulant, cannabin, other_drug)
  # conditions for alcohol test
  alc_cond <- 00:94
  # age bins
  age_labels <- c("< 25", "25--44", "45--64", "65+")
  # begin reading and cleaning data
  temp_df <- read.dbf(new_file_name) %>%
    select(STATE, VEH_NO, AGE, SEX, PER_TYP, ALC_RES,
           DRUGRES1, DRUGRES2, DRUGRES3, INJ_SEV, LAG_HRS, LAG_MINS,
           ST_CASE) %>%
    # filters
    filter(STATE %in% c(06,15,17,33,44,54)) %>% #CA, HI, IL, NH, RI, WV
    filter((LAG_HRS*60 + LAG_MINS < 61)) %>% #Died in 1 hr or less
    filter(PER_TYP == 01) %>% #Driver
    filter(INJ_SEV == 4) %>% #Fatality
    #---------------start modifying the filtered data
    # add "year" as a variable"
    mutate(year = which_year) %>%
    # set up unique_id by binding ST_CASE and VEH_NO
    # works because there is only one driver for each vehicle in each accident
    mutate(unique_id = paste0(year, "-", ST_CASE,"-", VEH_NO)) %>%
    # change sex to factor **not quite right yet**
    mutate(SEX = ifelse(SEX %in% 8:9, NA, SEX),
           SEX = factor(SEX, levels = c(1, 2), 
                        labels = c("Male", "Female"))) %>%
    # doublecheck age and create agecat factor
    mutate(AGE = ifelse(which_year < 2009 & AGE > 97, NA, AGE)) %>%
    mutate(agecat = cut(AGE, breaks = c(0, 24, 44, 64, 120), 
                        labels = (age_labels))) %>%
    # create alcohol as a factor
    # true if BAC >= 0.01
    mutate(Alcohol = ifelse(ALC_RES %in% 01:94, "True", NA),
           Alcohol = ifelse(ALC_RES == 00, "False", Alcohol),
           Alcohol = as.logical(Alcohol)) %>%
    # screen drug result values for NAs
    mutate(DRUGRES1 = ifelse(DRUGRES1 %in% drug_cond, DRUGRES1, NA)) %>%
    mutate(DRUGRES2 = ifelse(DRUGRES2 %in% drug_cond, DRUGRES2, NA)) %>%
    mutate(DRUGRES3 = ifelse(DRUGRES3 %in% drug_cond, DRUGRES3, NA)) %>%
    # drop columns no longer needed
    select(-ST_CASE, -VEH_NO, -PER_TYP, -INJ_SEV, -LAG_HRS, -LAG_MINS, 
           -STATE, -AGE, -ALC_RES) %>%
    # rename columns for clarity
    rename(sex = SEX)
  #---------------------------------------------------------
  # setting up to remove redundant entries (excess "None" entries)
  # gather drug values and create factor for drug presence
  #---------------------------------------------------------
# setting up to remove redundant entries (excess "None" entries)
# gather drug values and create factor for drug presence
  gathered_df <- temp_df %>%
    tidyr::gather(drug_number, drug_type_raw, DRUGRES1, DRUGRES2, DRUGRES3) %>%
    dplyr::mutate(drug_type = ifelse(drug_type_raw == 1, "None", NA),
           drug_type = ifelse(drug_type_raw %in% narcotic, "Narcotics",
                                drug_type),
           drug_type = ifelse(drug_type_raw %in% depress, "Depressants", 
                                drug_type),
           drug_type = ifelse(drug_type_raw %in% stimulant, "Stimulants",
                                drug_type),
           drug_type = ifelse(drug_type_raw %in% cannabin, "Cannabinoids",
                                drug_type),
           drug_type = ifelse(drug_type_raw %in% other_drug, "Other",
                                drug_type),
           drug_type = factor(drug_type)) %>%
    dplyr::select(-drug_type_raw, -drug_number) %>%
    dplyr::filter(!(is.na(drug_type) & (is.na(Alcohol))))
# non missing values
  non_missing_drugs <- gathered_df %>%
    filter(!is.na(drug_type)) %>%
    group_by(unique_id, drug_type) %>%
    summarize(has_drug = TRUE) %>%
    ungroup() %>%
    mutate(row_num = 1:n()) %>%
    spread(drug_type, has_drug, fill = FALSE) %>%
    select(-row_num) %>%
    group_by(unique_id) %>%
    summarize(Cannabinoids = any(Cannabinoids),
              Depressants = any(Depressants),
              Narcotics = any(Narcotics),
              Other = any(Other),
              Stimulants = any(Stimulants)) %>%
    ungroup()
#######---------------------
  temp_df <- temp_df %>%
    dplyr::select(-DRUGRES1, -DRUGRES2, -DRUGRES3) %>%
    dplyr::left_join(non_missing_drugs, by = "unique_id", r) %>%
    tidyr::gather(drug_type, positive_for_drug, Alcohol, Cannabinoids, 
          Depressants, Narcotics, Other, Stimulants) %>%
    dplyr::mutate(drug_type = factor(drug_type)) %>%
    dplyr::mutate(positive_for_drug = as.logical(positive_for_drug))
  return(temp_df)
}



# loop through all files to clean the data
for(study_year in 1999:2010) {
  df <- clean_yearly_person_file(study_year)
  if(study_year == 1999){
    clean_fars <- df
  } else {
    clean_fars <- rbind(clean_fars, df)
  }
}
save(clean_fars, file = "data/clean_fars.RData")
