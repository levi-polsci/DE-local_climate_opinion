# Title     : Local climate change attitude
# Objective : Calculate local estimates for variouse climate change attitudes from the SNB data
# Created by: s.levi
# Created on: 13/12/2021

wd <- "C:/Users/s.levi/OneDrive - Hertie School/Original Articles/Unpublished/Local attitudes"
setwd(wd)
source("Code/local_initiate.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Preprocess data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(FALSE){
  # 1a. Obtain and preprocess data
  #################################
  individualData <- createIndividualData(disaggregate_cities=FALSE) # obtain and preprocess individual data; optional value to assign gkz for districts in Hamburg, Berlin and Munich based on PLZ
  individualData_heat <- createIndividualData_heat(disaggregate_cities=FALSE)
  context_data <- createContextData(level = "landkreis") # obtain and preprocess context level data; levels currently implemented: 'landkreis' and 'bundesland'
  census <- getCensusData(level="landkreise", disaggregate_cities=FALSE) # Obtain census data; levels currently implemented: 'landkreis'

  # For Landkreise, check if everything is complete
  checkContextComplete(ind=individualData, cont = context_data, level = "landkreis") # Check if context-level data cover all Landkreise present in SNB
  checkContextComplete(ind=individualData_heat, cont = context_data, level = "landkreis") # Check if context-level data cover all Landkreise present in Heat data

  # For Kommunen, select all kommunen with at least 3 obs and for which context info is available (aka more than 10k inhabitants)
  # Exchange Berlin
  individualData[individualData$bl_kz=="11", "gkz_f"] <- paste0(substr(individualData[individualData$bl_kz=="11", "gkz_f"],1,5), "000")
  unit_check <- individualData %>% mutate(count=1) %>% collap(count ~ gkz_f + Gemeindename, FUN = fsum) %>% filter(count>2)
  #unit_check$gkz_f <- paste0(as.character(unit_check$gkz_f), "0000")
  View(collap(census, count ~ gkz_f+ gem_name))
  ids <- unit_check[unit_check$gkz_f %in% unique(as.character(census$gkz_f)), "gkz_f"]
  length(ids)


  # 1b. Impute missing data
  ################################

  # For SNB impute
  individualData_imputed <- imputeNAs(individualData, context_data, target = "individual") # impute individual data with context data and returning small dataframe
  saveRDS(individualData_imputed, file = "Data/individualData_20220524.rds") # save imputed dataset

  # For Heat case-wise delete
  individualData_heat_complete <- individualData_heat %>% filter_at(vars(age:lk_kz),all_vars(!is.na(.))) %>%
    rename(educ_cat =educ2) %>% rename(age_cat = age)
  individualData_heat_complete$educ_cat <- as.factor(individualData_heat_complete$educ_cat)
  individualData_heat_complete$gender <- as.factor(individualData_heat_complete$gender)

  contextData_imputed <- imputeNAs(ind=individualData, cont=context_data, target = "context", mode=2) # impute context data with individual data; mode 1: only impute years w/o missing values; mode2: impute all years; recommendation: use mode 1 for SNB and mode 2 for wärmepanel
  contextData_imputed$heizungen <- as.numeric(contextData_imputed$heizungen)
  contextData_imputed <- missRanger(contextData_imputed)
  saveRDS(contextData_imputed, file = "Data/contextData_20220503.rds") # save imputed dataset

  # 1c. Merge context data to census and individual data
  ##########################################################
  individualData_imputed <- readRDS("Data/individualData_20220524.rds")
  contextData_imputed <- readRDS("Data/contexts/contextData_20220503.rds")
  contextData_imputed[contextData_imputed$year==2020, "year"]<- 2021

  # Here, you join the individual level data by both year and landkreis to the individual dataset
  ind <- left_join(individualData_imputed, contextData_imputed)
  ind_heat <- left_join(individualData_heat_complete, contextData_imputed[contextData_imputed$year==2021,], by = "lk_kz")

  # You create a census level dataset for each year, with the respective yearly context level data
  census17 <- join_all(list(census, collap(contextData_imputed %>% filter(year==2017) %>% select(-year), ~ lk_kz), collap(ind, bl_kz ~ lk_kz)), by="lk_kz", type = "left") %>% relocate(bl_kz, .after = lk_kz)
  census18 <- join_all(list(census, collap(contextData_imputed %>% filter(year==2018) %>% select(-year), ~ lk_kz), collap(ind, bl_kz ~ lk_kz)), by="lk_kz", type = "left") %>% relocate(bl_kz, .after = lk_kz)
  census19 <- join_all(list(census, collap(contextData_imputed %>% filter(year==2019) %>% select(-year), ~ lk_kz), collap(ind, bl_kz ~ lk_kz)), by="lk_kz", type = "left") %>% relocate(bl_kz, .after = lk_kz)
  census21 <- join_all(list(census, collap(contextData_imputed %>% filter(year==2021) %>% select(-year), ~ lk_kz), collap(ind, bl_kz ~ lk_kz)), by="lk_kz", type = "left") %>% relocate(bl_kz, .after = lk_kz)

  save(ind, ind_heat, census17, census18, census19,census21, file = "Data/input_lk_20220524.rdata")

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. Fit model and refine results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Run on GPU
#source("Code/local_runMrP.R")

# Consolidate data
#source("Code/local_consolidateResults.R")

# Create maps
#source("Code/local_CreateMap.R")

# current run: 57161

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. Create Figures
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Figure 1 - descriptive four plots

# Figure 2 - Figure w/ change over time

