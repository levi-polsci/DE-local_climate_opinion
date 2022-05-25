consolidateResults <- function (context=NULL, ind =NULL){

  if(is.null(context))
      context <- readRDS("Data/contexts/contextData_20220503.rds")
  if(is.null(ind))
    ind <- readRDS("Data/individualData_20220524.rds")

  context <- context %>% filter(year!=2016) %>% rename(ags=lk_kz)
  ind <- ind %>% collap(Landkreisname + Bundesland ~ lk_kz) %>% rename(ags=lk_kz)
  # -- obtain list of all files in the folder
  filenames_in <- list.files("Data/results/output")
  ttl_map <- read.xlsx("Data/title_data.xlsx")
  ttl_map$subtitle_EN <- gsub("xxx", "", ttl_map$subtitle_EN )

  # create empty dataframe object
  results_lk <- as.data.frame(matrix(NA, nrow = nrow(context), ncol = 2+length(unique(ls_vars))))
  colnames(results_lk) <- c("ags","year",unique(ls_vars))
  results_lk$ags <- context$ags
  results_lk$year <- context$year
  results_lk[results_lk$year==2020, "year"]<-2021

  # fill in loop

  for(i in seq_along(filenames_in)){
    load(paste0("Data/results/output/",filenames_in[i]))
    #data <- readRDS(paste0("Data/results/",filenames_in[i])) # -- load data
    data <- ml_model

    # -- join map with data
    predictions <- data$ebma %>% dplyr::rename(ags=lk_kz, var=ebma)
    predictions$ags <- as.character(predictions$ags)
    predictions[predictions$ags=="03159", "ags"]<-"03152" # correct lk_kz for Göttingen
    predictions <- rbind(predictions, predictions %>% filter(ags=="03152") %>%mutate(ags="03156")) # Add record for Osterrode which was formerly merged with Göttigngen

    yr <- sub(".*-","", sub(".rds.*", "",filenames_in[i]))
    var <- sub("*fittedModel_y.","", sub("-.*", "",filenames_in[i]))

    results_lk[results_lk$year==yr,colnames(results_lk)==var] <- unlist(lapply(pull(results_lk[results_lk$year==yr, ],"ags"), function (x) as.numeric(predictions[predictions$ags==x,"var"])))

  }

  results_lk <- results_lk %>% filter(if_any(bike:transmissionline_expansion, ~ !is.na(.))) %>% # filter rows with at least non-NA
    pivot_longer(bike:transmissionline_expansion, names_to = "var") %>% # convert into long format
    left_join(ttl_map[,c("var","subtitle_EN")]) %>% left_join(ind) %>% left_join(context)# attach variable description, landkreisname & context

  results_lk[is.na(results_lk$Bundesland), "Bundesland"]<-"Schleswig-Holstein"

  saveRDS(results_lk, "Data/results/results_lk.rds")
  write.xlsx(results_lk, "Data/results/results_lk.xlsx")
  print("Results saves as one dataframe.")


}