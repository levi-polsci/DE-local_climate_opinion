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
load("Data/results/input/input_kommune_20220826.rdata")
vars <- openxlsx::read.xlsx("Data/variable_info.xlsx")

# 2. Define variables
########################################

#context_predictors <-c("population","purchase_power","nr_cars","unemployment_rate",
#             "braunkohle","solare_strahlungsenergie","wind","gruene2", "afd2", "fdp2")

preds_coal <-c("population","purchase_power",#"nr_cars",#"unemployment_rate",
             "braunkohle",#"solare_strahlungsenergie",#wind,
                       "gruene2", "afd2")

preds_cars <-c("population","purchase_power","nr_cars","unemployment_rate",
#             "braunkohle","solare_strahlungsenergie","wind",
               "gruene2", "afd2")

preds_wind <-c("population","purchase_power","unemployment_rate",
             "solare_strahlungsenergie","wind",
               "gruene2", "afd2", "fdp2")

preds_solar <-c("population","purchase_power","unemployment_rate",
             "solare_strahlungsenergie","wind",
               "gruene2", "afd2", "fdp2")

preds_generic <-c("population","purchase_power","unemployment_rate",
             "nr_cars","wind","braunkohle", "gruene2", "afd2", "fdp2")

context_predictors <- preds_generic

#outcomes <- c('coal_exit','emobility_expansion','onshore', 'protest.wind', 'solarplant', 'ice', 'evsubs', 'cityban','bike', 'opnv','speedlimit')
outcomes <- c('ctax', 'climate.human')

# invest_pv
# 'invest_res',


for (y in outcomes){
  outcome <- paste0("y.", y)
  years <- numeric()

  census <- census %>% #collap(census, ~ bl_kz + age_cat + gender + educ_cat) %>%
    select(gkz_f, lk_kz, gender, educ_cat, age_cat,proportion ,all_of(context_predictors)) %>%
      mutate(across(all_of(c("gkz_f","lk_kz", "gender", "educ_cat", "age_cat")), as.factor))

  # Drop NA in outcome and create binary outcome variable
  ind_this <- ind %>%  drop_na(all_of(outcome)) %>% addBinaryOutcomeVariables(var=outcome, kommune = TRUE) %>%
    select(gkz_f, lk_kz, gender, educ_cat, age_cat,all_of(outcome), all_of(context_predictors))%>%
    mutate(across(all_of(c("gkz_f","lk_kz")), as.factor))

  # Run model to create estimates
  start_time <- Sys.time()

  fileConn<-file("start.txt")
  writeLines(c("Estimating ", outcome, " for kommune - level. Process started at ",as.character(start_time)), fileConn)
    close(fileConn)


  ml_model <- auto_MrP(
    y = outcome,    L1.x = c("gender","educ_cat","age_cat"),  L2.x = context_predictors,
    L2.unit = "gkz_f",
    #L2.reg="lk_kz",
    survey = ind_this,  census = census,
    bin.proportion = "proportion",
    cores = 120,
    verbose = TRUE,
    best.subset = TRUE, # best subset; works with pca and mrp
    ebma.n.draws=50,
    ebma.tol=c(0.01,0.005,0.001, 0.0005),
    lasso = TRUE, # LASSO;
    lasso.n.iter = 50, # default is 100
    pca=TRUE, # PCA;
    gb=FALSE, # Doesn't work
    gb.interaction.depth = c(1,2),
    gb.shrinkage = c(0.04,0.01, 0.005),
    gb.n.trees.init = 50,
    gb.n.trees.increase = 50,
    gb.n.trees.max = 400,
    gb.n.minobsinnode = 20,
    svm = FALSE, # Works! (with kernel="linear")
    svm.kernel = "linear",
    mrp = TRUE # works as well
    )
    end_time <- Sys.time()
    end_time - start_time #

    filename <- paste0("Data/fittedModel_", outcome, "- kommune.rds")
    save(ml_model, file = filename)

    fileConn<-file("end.txt")
    writeLines(c("File completed. Running time",as.character((end_time - start_time))), fileConn)
    close(fileConn)
}





