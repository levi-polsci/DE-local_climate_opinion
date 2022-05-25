scaleBerlinCensus <- function (data, kommune=TRUE){

  # Obtain weights
  weights <- read.xlsx("Data/census/census_euduc_berlin_dist.xlsx") %>% filter(GKZ != 11000000)

  # split Berlin census data from rest census data and create empty df for Berlin districs
  if(kommune){
    census_bln <- data[data$gkz_f=="110000000000",]
    census_other <- data[data$gkz_f!="110000000000",]
    census_bln_districs <- data[FALSE,]

  }else{
    census_bln <- data[data$lk_kz=="11000",]
    census_other <- data[data$lk_kz!="11000",]
    census_bln_districs <- data[FALSE,]
  }
  if(!kommune)
    stop("City weighting is currently only implemented for kommune level.")

  for (i in seq_len(nrow(weights))){
    # Update generic information
    census_district <- census_bln
    census_district$gkz_f <- as.factor(as.character(paste0(weights[i,"GKZ"],"0000")))
    census_district$gem_name <- paste0("Berlin-",weights[i, "Bezirk"])
    census_district$gem_population <- -1

    # Weight proportion
    census_district[census_district$educ_cat==1, "proportion"] <- census_district[census_district$educ_cat==1, "proportion"] *
      (weights[i,"EDUC1"]/sum(census_district[census_district$educ_cat==1, "proportion"])) # ohne berufsausbildung
    census_district[census_district$educ_cat==2, "proportion"] <- census_district[census_district$educ_cat==2, "proportion"] *
      (weights[i,"EDUC2"]/sum(census_district[census_district$educ_cat==2, "proportion"])) # berufsausbildung
    census_district[census_district$educ_cat %in% c(3,4), "proportion"] <- census_district[census_district$educ_cat %in% c(3,4), "proportion"] *
      (weights[i,"EDUC3"]/sum(census_district[census_district$educ_cat %in% c(3,4), "proportion"])) # fachhochschule
    census_district[as.numeric(as.character(census_district$educ_cat))>4, "proportion"] <- census_district[as.numeric(as.character(census_district$educ_cat))>4, "proportion"] *
      (weights[i,"EDUC5"]/sum( census_district[as.numeric(as.character(census_district$educ_cat)) >4, "proportion"])) # uni
    if(round(sum(census_district$proportion),10)!=1) stop(paste0("Inconsistent weighting error in ", weights[i, "Bezirk"],"!"))

    census_bln_districs <- rbind(census_bln_districs, census_district)
  }

  census <- rbind(census_other, census_bln_districs)

  return(census)

}