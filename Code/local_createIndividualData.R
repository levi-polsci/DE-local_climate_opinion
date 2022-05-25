createIndividualData <- function (disaggregate_cities=FALSE){

  print("Reading raw individual level data....")
  if(disaggregate_cities) print("Disaggregating districts for Berlin and Hamburg.") else print("Big cities lumped together.")

 data<- read_dta("Data/individual/DataMaster_2017-2019_GESIS_06.12.21.dta") %>% mutate_if(is.numeric, as.numeric) %>%
   add2021data() %>%
   addPLZ(disaggregate_cities) %>% # add PLZ
    preProcessSurvey() %>% # data cleaning
    addAreaCode(disaggregate_cities) %>% # attach gemeindekennzahl, landkreiskennzahl, bundesland, wahlkreis
    addCensusVars() # add data corresponding to the survey

}