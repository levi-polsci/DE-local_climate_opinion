#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculation file - to be executed on GPU
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Initialize libaries and obtain data
########################################
library(autoMrP)
library(parallel)
library(dplyr)
library(magrittr)
library(collapse)
library(tidyr)
library(car)
library(openxlsx)
source("Code/local_addBinaryOutcomeVariables.R")
load("Data/input_lk_20220524.rdata")
vars <- openxlsx::read.xlsx("Data/variable_info.xlsx")

# 2. Define variables
########################################

X_transport <- c("kfz_insgesamt", "kfz_pkw","wirtsch_abs_gesamt","wirtsch_abs_verkehr",
                                "haushalte_mit_hohem_einkommen_percent" , "bodenfl_siedlungha",
                 "wahlberechtigte","gruene2", "afd2", "fdp2")

X_energy <-c("wirtsch_abs_gesamt","wirtsch_abs_energie","bodenfl_siedlungha","bodenfl_insg",
             "braunkohle","solare_strahlungsenergie","wind","gruene2", "afd2", "fdp2")

X_Ctax <-c("kfz_insgesamt","wirtsch_abs_bergbau","haushalte_mit_niedrigem_einkommen_percent",  "hochqualifizierte_am_wohnort_percent","bodenfl_siedlungha", "wahlberechtigte","gruene2", "afd2", "fdp2")

X_generic <-c("kfz_pkw","wirtsch_abs_gesamt","beschaftigungsquote_percent",  "hochqualifizierte_am_wohnort_percent", "bodenfl_siedlungha", "braunkohle", "gruene2", "afd2", "fdp2")

X_heat <- c("wirtsch_abs_gesamt","haushalte_mit_niedrigem_einkommen_percent", "gruene2", "afd2", "wohnflaeche_p_p", "fert_wohnungen_ee","fernheizung_fernwarme","heizungen","blockheizung", "fdp2")

outcomes <- c('geothermal')

for (y in outcomes){
  outcome <- paste0("y.", y)
  years <- numeric()
  if(vars[vars$outcome==outcome, "available.2017"]) years <- 2017
  if(vars[vars$outcome==outcome, "available.2018"]) years <- c(years, 2018)
  if(vars[vars$outcome==outcome, "available.2019"]) years <- c(years, 2019)
    if(vars[vars$outcome==outcome, "available.2021"]) years <- c(years, 2021)

  if(vars[vars$outcome==outcome,"type"]=="generic") context_predictors <- X_generic
  if(vars[vars$outcome==outcome,"type"]=="transport") context_predictors <- X_transport
  if(vars[vars$outcome==outcome,"type"]=="energy") context_predictors <- X_energy
  if(vars[vars$outcome==outcome,"type"]=="ctax") context_predictors <- X_Ctax
    if(vars[vars$outcome==outcome,"type"]=="heat") context_predictors <- X_heat

  for (yr in years){
    if(yr==2019) census <- census19
    if(yr==2018) census <-census18
    if(yr==2017) census <- census17
      if(yr==2021) census <- census21

    census <- census %>% #collap(census, ~ bl_kz + age_cat + gender + educ_cat) %>%
      select(lk_kz, bl_kz, gender, educ_cat, age_cat,proportion ,all_of(context_predictors)) %>%
      mutate(across(all_of(c("lk_kz","bl_kz","gender", "educ_cat", "age_cat")), as.factor))

      # Specify year, drop NA in outcome, and create binary outcome variable
    if(all(context_predictors == X_heat)){
        ind_this <- ind_heat %>%  drop_na(all_of(outcome)) %>% addBinaryOutcomeVariables(var=outcome) %>%
      select(lk_kz, bl_kz, gender, educ_cat, age_cat,all_of(outcome), all_of(context_predictors))%>%
      mutate(across(all_of(c("lk_kz","bl_kz")), as.factor))
    }else{
      ind_this <- ind %>% filter(year==yr) %>%  drop_na(all_of(outcome)) %>% addBinaryOutcomeVariables(var=outcome) %>%
      select(lk_kz, bl_kz, gender, educ_cat, age_cat,all_of(outcome), all_of(context_predictors))%>%
      mutate(across(all_of(c("lk_kz","bl_kz")), as.factor))
        }

    # Run model to create estimates
    start_time <- Sys.time()

    fileConn<-file("start.txt")
    writeLines(c("Estimating ", outcome, " for year", as.character(yr), ". Process started at ",as.character(start_time)), fileConn)
    close(fileConn)


  ml_model <- auto_MrP(
    y = outcome,    L1.x = c("gender","educ_cat","age_cat"),  L2.x = context_predictors,
    L2.unit = "lk_kz",
    L2.reg="bl_kz",
    survey = ind_this,  census = census,
    bin.proportion = "proportion",
    cores = 120,
    verbose = TRUE,
    best.subset = TRUE, # best subset; works with pca and mrp
    ebma.n.draws=80,
    ebma.tol=c(0.01,0.005,0.001, 0.0005, 0.0001),
    lasso = TRUE, # LASSO;
    lasso.n.iter = 50, # default is 100
    pca=TRUE, # PCA;
    gb=TRUE, # Works!
    gb.interaction.depth = c(1,2,3),
    gb.shrinkage = c(0.04,0.01, 0.008, 0.005),
    gb.n.trees.init = 50,
    gb.n.trees.increase = 50,
    gb.n.trees.max = 500,
    gb.n.minobsinnode = 50,
    svm = TRUE, # Works! (with kernel="linear")
    svm.kernel = "linear",
    mrp = TRUE # works as well
    )
    end_time <- Sys.time()
    end_time - start_time #

    filename <- paste0("Data/fittedModel_", outcome, "-",as.character(yr),".rds")
    save(ml_model, file = filename)

    fileConn<-file("end.txt")
    writeLines(c("File completed. Running time",as.character((end_time - start_time))), fileConn)
    close(fileConn)
  }
}





