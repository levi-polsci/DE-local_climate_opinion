scaleHamburgCensus <- function (data){

   # Obtain weights
  weights <- read.xlsx("Data/census/Zensus Bildungsdaten Hamburg.xlsx", sheet = "Hamburg_formatted") %>% filter(GKZ != 02000000)
  if(!kommune)
    stop("City weighting is currently only implemented for kommune level.")

  # split Hamburg census data from rest census data and create empty df for Berlin districs
  census_ham <- data[data$gkz_f=="020000000000",]
  census_other <- data[data$gkz_f!="020000000000",]
  census_ham_districs <- data[FALSE,]

  for (i in seq_len(nrow(weights))){
    # Update generic information
    census_district <- census_ham
    census_district$gkz_f <- as.factor(as.character(paste0(weights[i,"GKZ"],"0000000")))
    census_district$gem_name <-weights[i, "Bezirk"]
    census_district$gem_population <- -1

    # Weight proportion
    census_district[census_district$educ_cat==1, "proportion"] <- census_district[census_district$educ_cat==1, "proportion"] *
      (weights[i,"educ_1"]/sum(census_district[census_district$educ_cat==1, "proportion"])) # ohne berufsausbildung
    census_district[census_district$educ_cat==2, "proportion"] <- census_district[census_district$educ_cat==2, "proportion"] *
      (weights[i,"educ_2"]/sum(census_district[census_district$educ_cat==2, "proportion"])) # berufsausbildung
    census_district[census_district$educ_cat==3, "proportion"] <- census_district[census_district$educ_cat==3, "proportion"] *
      (weights[i,"educ_3"]/sum(census_district[census_district$educ_cat==3, "proportion"])) # fachschule
    census_district[census_district$educ_cat==4, "proportion"] <- census_district[census_district$educ_cat==4, "proportion"] *
      (weights[i,"educ_4"]/sum(census_district[census_district$educ_cat==4, "proportion"])) # fachakademie
    census_district[census_district$educ_cat==5, "proportion"] <- census_district[census_district$educ_cat==5, "proportion"] *
      (weights[i,"educ_5"]/sum(census_district[census_district$educ_cat==5, "proportion"])) # fachholschule
    census_district[census_district$educ_cat==6, "proportion"] <- census_district[census_district$educ_cat==6, "proportion"] *
      (weights[i,"educ_6"]/sum(census_district[census_district$educ_cat==6, "proportion"])) # hochschule
    census_district[census_district$educ_cat==7, "proportion"] <- census_district[census_district$educ_cat==7, "proportion"] *
      (weights[i,"educ_7"]/sum(census_district[census_district$educ_cat==7, "proportion"])) # phd

    if(round(sum(census_district$proportion),10)!=1) stop(paste0("Inconsistent weighting error in ", weights[i, "Bezirk"],"!"))

    census_ham_districs <- rbind(census_ham_districs, census_district)
  }

  census <- rbind(census_other, census_ham_districs)

  return(census)


}