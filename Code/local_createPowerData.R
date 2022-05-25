createPowerData <- function (level, disaggregate_cities=FALSE){

  options(scipen = 999) # supress scientific notation
  ifelse(disaggregate_cities, file <- "Data/contexts/power/Version2_Berlin/Kraftwerke_aggregiert_V2.rds", file <- "Data/contexts/power/Kraftwerke_aggregiert.rds")
  kw_data <- readRDS(file) %>% clean_names() %>% # Read and rename columns
    rename(tech=energietrager, capacity=bruttoleistung_der_einheit)

  if(!disaggregate_cities) colnames(kw_data)[3] <- "gkz"


  kw_data$gkz_f <- as.factor(ifelse(kw_data$gkz < 10000000, paste0("0", as.character(kw_data$gkz)),as.character(kw_data$gkz)))
  kw_data$lk_kz <- as.factor(substr(as.character(kw_data$gkz_f), 1,5))

  if(!(level %in% c("landkreis", "kommune")))
    stop("Please specify level as either 'landkreis' or 'kommune'")

  if(level=="landkreis"){
    suppressWarnings(
      kw_data <- kw_data %>%
        spread(tech, capacity) %>% # create dummies
        select(-c(gkz, gkz_f)) %>% # subset
        collap( ~ lk_kz, FUN = fsum) %>%  # aggregate
       replace(is.na(.), 0) # replace "." with zero
     )
  }
  if(level=="kommune"){
    suppressWarnings(
      kw_data <- kw_data %>%
        spread(tech, capacity) %>% # create dummies
        select(-c(gkz, lk_kz)) %>% # subset
        collap( ~ gkz_f, FUN = fsum) %>%  # aggregate
       replace(is.na(.), 0) # replace "." with zero
     )
  }

  if(disaggregate_cities){
     #Add Hamburg
    district <- kw_data[!is.na(kw_data$gkz_f) & kw_data$gkz_f=="02000000",]
    district[,2:20] <- district[,2:20] /7
    for (i in 1:7){
      district$gkz_f <-  paste0("0200", as.character(i),"000")
      kw_data <- rbind(kw_data, district)
    }
    # Add München
    district <- kw_data[!is.na(kw_data$gkz_f) & kw_data$gkz_f=="09162000",]
    district[,2:20] <- district[,2:20] /25
    for (i in 1:25){
      if(i<10)
        district$gkz_f <-  paste0("0916200", as.character(i)) else
        district$gkz_f <-  paste0("091620", as.character(i))
      kw_data <- rbind(kw_data, district)
    }
  }

  if(level=="kommune")
    kw_data <- kw_data[!is.na(kw_data$gkz_f),] else
    kw_data <- kw_data[!is.na(kw_data$lk_kz),]
  return(kw_data)

}