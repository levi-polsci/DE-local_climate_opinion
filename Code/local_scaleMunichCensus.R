scaleMunichCensus <- function (data){

     # Obtain weights
  weights <- read.xlsx("Data/census/munich_census_age.xlsx", sheet = "Munich_share")
  if(!kommune)
    stop("City weighting is currently only implemented for kommune level.")

  # split Hamburg census data from rest census data and create empty df for Berlin districs
  weights$gkz <- paste0("0", as.character(weights$gkz))
  census_mun <- data[data$gkz_f=="091620000000",]
  census_other <- data[data$gkz_f!="091620000000",]
  census_mun_districs <- data[FALSE,]

  for (i in seq_len(nrow(weights))){
    # Update generic information
    census_district <- census_mun
    census_district$gkz_f <- as.factor(as.character(paste0(weights[i,"gkz"],"0000"))) # TODO! CHANGE!
    census_district$gem_name <-paste0("Muenchen, ", weights[i, "Stadtteil"])
    census_district$gem_population <- -1

    # Weight proportion
    census_district[census_district$age_cat==1, "proportion"] <- census_district[census_district$age_cat==1, "proportion"] *
      (weights[i,"age_1"]/sum(census_district[census_district$age_cat==1, "proportion"])) # 18-24
    census_district[census_district$age_cat==2, "proportion"] <- census_district[census_district$age_cat==2, "proportion"] *
      (weights[i,"age_2"]/sum(census_district[census_district$age_cat==2, "proportion"])) # 25-29
    census_district[census_district$age_cat==3, "proportion"] <- census_district[census_district$age_cat==3, "proportion"] *
      (weights[i,"age_3"]/sum(census_district[census_district$age_cat==3, "proportion"])) # 30-39
    census_district[census_district$age_cat==4, "proportion"] <- census_district[census_district$age_cat==4, "proportion"] *
      (weights[i,"age_4"]/sum(census_district[census_district$age_cat==4, "proportion"])) # 40-49
    census_district[census_district$age_cat==5, "proportion"] <- census_district[census_district$age_cat==5, "proportion"] *
      (weights[i,"age_5"]/sum(census_district[census_district$age_cat==5, "proportion"])) # 50-64
    census_district[census_district$age_cat==6, "proportion"] <- census_district[census_district$age_cat==6, "proportion"] *
      (weights[i,"age_6"]/sum(census_district[census_district$age_cat==6, "proportion"])) # 65-75
    census_district[census_district$age_cat==7, "proportion"] <- census_district[census_district$age_cat==7, "proportion"] *
      (weights[i,"age_7"]/sum(census_district[census_district$age_cat==7, "proportion"])) # 75-100

    if(round(sum(census_district$proportion),10)!=1) stop(paste0("Inconsistent weighting error in ", weights[i, "Bezirk"],"!"))

    census_mun_districs <- rbind(census_mun_districs, census_district)
  }

  census <- rbind(census_other, census_mun_districs)

  return(census)


}