createIndividualData_heat <- function (disaggregate_cities=FALSE){

  print("Reading raw individual level data....")
  if(disaggregate_cities) print("Disaggregating districts for Berlin and Hamburg.") else print("Big cities lumped together.")

 data<- survey_raw <- read_dta("Data/individual/sommer_ariadne_1.dta") %>%
    preProcessSurvey_heat() %>% # data cleaning
    addAreaCode(disaggregate_cities)

}