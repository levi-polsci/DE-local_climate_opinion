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

preds_coal <-c("wahlberechtigte", "beschaftigungsquote_percent","wirtsch_abs_gesamt",
             "braunkohle", "bodenfl_insg", "gruene2", "afd2", "fdp2")

preds_cars <-c("wahlberechtigte", "beschaftigungsquote_percent","kfz_pkw","beschaftigungsquote_percent",
                 "bodenfl_insg","gruene2", "afd2", "fdp2")

preds_wind <-c("wahlberechtigte", "beschaftigungsquote_percent","wirtsch_abs_gesamt",
             "wind", "bodenfl_insg", "gruene2", "afd2", "fdp2")

context_predictors <- preds_wind

#outcomes <- c('coal_exit','emobility_expansion','onshore', 'protest.wind')
outcomes <- c('onshore', 'protest.wind')

for (y in outcomes){
  outcome <- paste0("y.", y)

  census <- census18

    census <- census %>% #collap(census, ~ bl_kz + age_cat + gender + educ_cat) %>%
      select(lk_kz, bl_kz, gender, educ_cat, age_cat,proportion ,all_of(context_predictors)) %>%
      mutate(across(all_of(c("lk_kz","bl_kz","gender", "educ_cat", "age_cat")), as.factor))

      ind_this <- ind %>%  drop_na(all_of(outcome)) %>% addBinaryOutcomeVariables(var=outcome) %>%
      select(lk_kz, bl_kz, gender, educ_cat, age_cat,all_of(outcome), all_of(context_predictors))%>%
      mutate(across(all_of(c("lk_kz","bl_kz")), as.factor))


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

    filename <- paste0("Data/fittedModel_", outcome, "-avg_landkreis_for_kommune.rds")
    save(ml_model, file = filename)

    fileConn<-file("end.txt")
    writeLines(c("File completed. Running time",as.character((end_time - start_time))), fileConn)
    close(fileConn)

}





