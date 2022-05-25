
createContextData <- function (level="landkreis", disaggregate_cities=FALSE){

  if(!(level %in% c("kommune","landkreis", "bundesland"))) stop("Currently, context level data can only delivered at the Landkreis and Bundesland level") else
    print(paste0("Collecting data on the ", level, " level:"))

  if(level=="kommune"){
    disaggregate_cities <- TRUE
    print("Collecting kommmune-level data...")
    kw_data <- createPowerData(level, disaggregate_cities) # Obtain power data
    election_data <- getElectionData(level, disaggregate_cities)
    other_data <- getOtherKommuneData()
       # - Merge all types of context level data
    context_kommune <- join_all(list(other_data, election_data, kw_data), by="gkz_f", type = "left") %>% clean_names()
    context_kommune[context_kommune$nr_cars<0, "nr_cars"]<-0
    context <- context_kommune


  } else{
    print("Collecting socio-demographic data...")
    socioDemoData <- addSocioDemoData(level, disaggregate_cities)

    print("Collecting capacity data...")
    kw_data <- createPowerData(level, disaggregate_cities) # Obtain power data

    print("Collecting election data...")
    election_data <- getElectionData(level, disaggregate_cities)

    print("Collecting heat data")
    heat_data <- getHeatData()

    # - Merge all types of context level data
    context <- join_all(list(socioDemoData, kw_data, election_data, heat_data), by="lk_kz", type = "left") %>% clean_names()

  if(level=="bundesland"){
    context$bl_kz <- as.factor(substr(as.character(context$lk_kz), 1,2))
    context <- context %>% select(-"lk_kz") %>% collap(~ bl_kz + year)
  }
  }



  context

}