# Title     : Local climate change attitude
# Objective : Calculate local estimates for variouse climate change attitudes from the SNB data
# Created by: s.levi
# Created on: 13/12/2021

wd <- "C:/Users/s.levi/OneDrive - Hertie School/Original Articles/Unpublished/Local attitudes"
setwd(wd)
source("Code/local_initiate.R")


#Sys.setlocale("LC_ALL", "en_US.utf8")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Preprocess data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(FALSE){
  # 1a. Obtain and preprocess data
  #################################
  individualData <- createIndividualData(disaggregate_cities=TRUE) # obtain and preprocess individual data; optional value to assign gkz for districts in Hamburg, Berlin and Munich based on PLZ
  individualData_heat <- createIndividualData_heat(disaggregate_cities=TRUE)
  context_data <- createContextData(level = "kommune") # obtain and preprocess context level data; levels currently implemented: 'kommune', 'landkreis' and 'bundesland'
  census <- getCensusData(level="kommune", disaggregate_cities=TRUE) # Obtain census data; levels currently implemented: 'landkreis', 'kommune'
  #saveRDS(census, "Data/census_kommune_20220608.rds")
  #census <-  readRDS("Data/census_kommune_20220608.rds")

  # For Landkreise, check if everything is complete
  checkContextComplete(ind=individualData, cont = context_data, level = "landkreis") # Check if context-level data cover all Landkreise present in SNB
  checkContextComplete(ind=individualData_heat, cont = context_data, level = "landkreis") # Check if context-level data cover all Landkreise present in Heat data

  # For Kommunen, select all kommunen with at least 3 obs and for which context info is available (aka more than 10k inhabitants)
  # Exchange Berlin
  individualData[individualData$bl_kz=="11", "gkz_f"] <- paste0(substr(individualData[individualData$bl_kz=="11", "gkz_f"],1,5), "000")
  #unit_check <- individualData %>% mutate(count=1) %>% collap(count ~ gkz_f + Gemeindename, FUN = fsum) %>% filter(count>2)
  #unit_check$gkz_f <- paste0(as.character(unit_check$gkz_f), "0000")
  #View(collap(census, count ~ gkz_f+ gem_name))
  #ids <- unit_check[unit_check$gkz_f %in% unique(as.character(census$gkz_f)), "gkz_f"] # ids of kommunen for which sufficient data is available
  #length(ids)



  # 1b. Impute missing data
  ################################

  # For SNB impute
  individualData_imputed <- imputeNAs(ind = individualData, cont = context_data, target = "individual", level = "landkreis") # impute individual data with context data and returning small dataframe
  saveRDS(individualData_imputed, file = "Data/individualData_kommune_20220906.rds") # save imputed dataset
  #individualData_imputed <- readRDS("Data/results/input/individualData_kommune_20220708.rds")

  # For Heat case-wise delete
  individualData_heat_complete <- individualData_heat %>% filter_at(vars(age:lk_kz),all_vars(!is.na(.))) %>%
    dplyr::rename(educ_cat =educ2) %>% dplyr::rename(age_cat = age)
  individualData_heat_complete$educ_cat <- as.factor(individualData_heat_complete$educ_cat)
  individualData_heat_complete$gender <- as.factor(individualData_heat_complete$gender)

  # For KOMMUNE
  contextData_imputed <- imputeNAs(ind=individualData, cont=context_data, target = "context", mode=1, level = "kommune") # impute context data with individual data; mode 1: only impute years w/o missing values; mode2: impute all years; recommendation: use mode 1 for SNB and mode 2 for wärmepanel
  contextData_imputed$heizungen <- as.numeric(contextData_imputed$heizungen) # only for landkreis level
  # For LANDKREIS
  contextData_imputed <- missRanger(contextData_imputed) # only for landkreis level
  saveRDS(contextData_imputed, file = "Data/contextData_kommune_20220712.rds") # save imputed dataset

  # 1c. Merge context data to census and individual data
  ##########################################################

  kommune <- TRUE

     if(!kommune){ # routine for landkreis
       #individualData_imputed <- readRDS("Data/results/input/individualData_20220708.rds")
       individualData_imputed <- readRDS("Data/results/input/individualData_landkreis_20220712.rds") %>%
         select(-y.ctax) %>% left_join(individualData %>% select(key, year, y.ctax)) # add c0

       #summary(as.factor(pull(individualData_imputed[individualData_imputed$year==2021,], "y.ctax")))

       contextData_imputed <- readRDS("Data/results/input/contextData_kommune_20220712.rds")

       # only for lk
       contextData_imputed[contextData_imputed$year==2020, "year"]<- 2021
       # Here, you join the individual level data by both year and landkreis to the individual dataset
       ind <- left_join(individualData_imputed, contextData_imputed)
       ind_heat <- left_join(individualData_heat_complete, contextData_imputed[contextData_imputed$year==2021,], by = "lk_kz")
       # You create a census level dataset for each year, with the respective yearly context level data
       census17 <- join_all(list(census, collap(contextData_imputed %>% filter(year==2017) %>% select(-year), ~ lk_kz), collap(ind, bl_kz ~ lk_kz)), by="lk_kz", type = "left") %>% relocate(bl_kz, .after = lk_kz)
       census18 <- join_all(list(census, collap(contextData_imputed %>% filter(year==2018) %>% select(-year), ~ lk_kz), collap(ind, bl_kz ~ lk_kz)), by="lk_kz", type = "left") %>% relocate(bl_kz, .after = lk_kz)
       census19 <- join_all(list(census, collap(contextData_imputed %>% filter(year==2019) %>% select(-year), ~ lk_kz), collap(ind, bl_kz ~ lk_kz)), by="lk_kz", type = "left") %>% relocate(bl_kz, .after = lk_kz)
       census21 <- join_all(list(census, collap(contextData_imputed %>% filter(year==2021) %>% select(-year), ~ lk_kz), collap(ind, bl_kz ~ lk_kz)), by="lk_kz", type = "left") %>% relocate(bl_kz, .after = lk_kz)
       save(ind, ind_heat, census17, census18, census19,census21, file = "Data/results/input/input_lk_20220826.rdata")


     }else{ # routine for kommune
       individualData_imputed <- readRDS("Data/individualData_kommune_20220906.rds")
       contextData_imputed <- readRDS("Data/results/input/contextData_kommune_20220607.rds")
       census <- readRDS("Data/results/input/census_kommune_20220608.rds")

       unit_check <- individualData_imputed %>% mutate(count=1) %>% collap(count ~ gkz_f + Gemeindename, FUN = fsum) %>% filter(count>2)
       ids <- pull(unit_check[unit_check$gkz_f %in% unique(as.character(census$gkz_f)), ],"gkz_f") # ids of kommunen for which sufficient data is available
       length(ids)

       # before merging, make sure that Berlin context data is in the same format as ind/census context data
       contextData_imputed[as.numeric(as.character(contextData_imputed$gkz_f))> 11000000 & as.numeric(as.character(contextData_imputed$gkz_f))<12000000, "gkz_f"] <- lapply(
         contextData_imputed[as.numeric(as.character(contextData_imputed$gkz_f))> 11000000 & as.numeric(as.character(contextData_imputed$gkz_f))<12000000, "gkz_f"], function (x)
           paste0(substr(as.character(x), start = 1, stop=5),"000"))
       # and the same for Munich
       census[as.numeric(as.character(census$gkz_f))> 916200 & as.numeric(as.character(census$gkz_f))<916300, "gkz_f"] <- lapply(
         census[as.numeric(as.character(census$gkz_f))> 916200 & as.numeric(as.character(census$gkz_f))<916300, "gkz_f"], function (x)
           paste0(substr(as.character(x), start = 1, stop=5),"0", substr(as.character(x), start = 6, stop=7)))

       ids[!(ids %in% contextData_imputed$gkz_f)] # check which individual ids are not present in context data - Walsrode 03358022 and 14521030 Aue (Erzgebirgskreis)

       individualData_imputed[individualData_imputed$gkz_f=="03358022", "gkz_f"]<- "03358024" # correct gkz_f for Walsrode (retired end of 2019) https://gov.genealogy.net/item/show/WALOD1JO42TU
       census[census$gkz_f=="03358022", "gkz_f"] <- "03358024"
       individualData_imputed[individualData_imputed$gkz_f=="14521030", "gkz_f"]<- "14521035" # Aue became part of the area Aue-Bad Schlema in 2019 https://de.wikipedia.org/wiki/Aue-Bad_Schlema
       census[census$gkz_f=="14521030", "gkz_f"] <- "14521035"

       # Re-do unit check and creation of ids
       unit_check <- individualData_imputed %>% mutate(count=1) %>% collap(count ~ gkz_f + Gemeindename, FUN = fsum) %>% filter(count>2)
       ids <- pull(unit_check[unit_check$gkz_f %in% unique(as.character(census$gkz_f)), ], "gkz_f") # ids of kommunen for which sufficient data is available
       length(ids)
       ids[!(ids %in% contextData_imputed$gkz_f)] # now all gkz's are present in individual data, context, and census
       individualData_imputed$gkz_f <- as.character(individualData_imputed$gkz_f)
       census$gkz_f <- as.character(census$gkz_f)

       # Check Munich Bezirke
       length(unique(individualData_imputed[substr(as.character(individualData_imputed$gkz_f), start = 1, stop=5)=="09162","gkz_f"]))
       nrow(unique(contextData_imputed[substr(as.character(contextData_imputed$gkz_f), start = 1, stop=5)=="09162","gkz_f"]))
       nrow(unique(census[substr(as.character(census$gkz_f), start = 1, stop=5)=="09162","gkz_f"]))

       # Here, you join the individual level data for which sufficient data is available at the kommune level to the individual dataset
       ind <- left_join(individualData_imputed %>% filter(gkz_f %in% ids),  contextData_imputed )
       # You create a census level dataset for each year, with the respective yearly context level data
       census <- join_all(list(census %>% filter(gkz_f %in% ids), contextData_imputed, collap(ind, lk_kz ~ gkz_f)), by="gkz_f", type = "left") %>% relocate(lk_kz, .after = gkz_f)

       individualData_imputed$gkz_f <- as.factor(individualData_imputed$gkz_f)
       census$gkz_f <- as.factor(census$gkz_f)
       save(ind, census, file = "Data/results/input/input_kommune_20220906.rdata")

       #load("Data/results/input/input_kommune_20220826.rdata")


  }



}


### Add co2 price 2021


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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6. Prepare data for app
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

