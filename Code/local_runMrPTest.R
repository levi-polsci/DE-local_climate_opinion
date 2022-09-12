library(autoMrP)
library(parallel)
library(dplyr)
library(magrittr)
library(collapse)
library(tidyr)
library(car)
source("Code/local_addBinaryOutcomeVariables.R")

# Load data
load("Data/ind_census_20220203.rdata")

# TODO: Test whether you can use integer values for outcome variable
# TODO: Test run with uncertainty margin
# TODO: Disaggregate Berlin
# Afterwards: Run through loop

# preprocess
context_predictors <- c("wirtsch_abs_gesamt", "gruene2", "afd2")


census_bl <- collap(census, ~ bl_kz + age_cat + gender + educ_cat) %>%
  select(lk_kz, bl_kz, gender, educ_cat, age_cat,proportion ,all_of(context_predictors)) %>%
 mutate(across(all_of(c("lk_kz","bl_kz","gender", "educ_cat", "age_cat")), as.factor))

# Specify year, drop NA in outcome, and create binary outcome variable
ind <- ind %>% filter(year==2019) %>%  drop_na(y.ctax) %>% addBinaryOutcomeVariables("y.ctax") %>%
      select(lk_kz, bl_kz, y.ctax, gender, educ_cat, age_cat,all_of(context_predictors))%>%     mutate(across(all_of(c("lk_kz","bl_kz")), as.factor))

#ind$y.ctax <- ind$y.ctax  *2

#context_predictors  <- colnames(ind %>% select(kfz_insgesamt:partei2))


# Run model to create estimates
start_time <- Sys.time()

fileConn<-file("start.txt")
writeLines(c("Process started at ",as.character(start_time)), fileConn)
close(fileConn)


ml_model <- auto_MrP(
  #y = "YES",  L1.x = c("L1x1", "L1x2", "L1x3"),  L2.x = c("L2.x1",   "L2.x2",   "L2.x3"),  L2.unit = "state",
  #survey = taxes_survey,  census = taxes_census,
  y = "y.ctax",    L1.x = c("gender","educ_cat","age_cat"),  L2.x = context_predictors,
    L2.unit = "bl_kz",
    #L2.reg="bl_kz",
  survey = ind,  census = census_bl,
  bin.proportion = "proportion",
  cores = 120,
  verbose = TRUE,
  best.subset = TRUE, # best subset; works with pca and mrp
  ebma.n.draws=80,
  ebma.tol=c(0.01,0.005,0.001),
  lasso = TRUE, # LASSO;
  lasso.n.iter = 10, # default is 100
  pca=TRUE, # PCA;
  gb=TRUE, # Works!
  gb.interaction.depth = c(1,2,3),
  gb.shrinkage = c(0.04,0.01, 0.008),
  gb.n.trees.init = 10,
  gb.n.trees.increase = 10,
  gb.n.trees.max = 50,
  gb.n.minobsinnode = 200,
  svm = TRUE, # Works! (with kernel="linear")
  svm.kernel = "linear",
  mrp = TRUE, # works as well
    uncertainty = TRUE, boot.iter=100
)
end_time <- Sys.time()
end_time - start_time #

save(ml_model, file = "Data/fittedModel_uncertainty.rdata")

fileConn<-file("end.txt")
writeLines(c("File completed. Running time",as.character((end_time - start_time))), fileConn)
close(fileConn)

