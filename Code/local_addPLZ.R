addPLZ <- function(data, execute){
  if(execute){
    options(scipen = 999) # supress scientific notation
    #plz17[duplicated(plz17$key),]
    #Data/Barometer_2017_key_plz.dta
    plz_data <- rbind(read_dta("Data/individual/Barometer_2017_key_plz.dta")%>% mutate(year=2017),
                      read_dta("Data/individual/SNB_2018_plz_key.dta")%>% mutate(year=2018),
                      read_dta("Data/individual/SNB_2019_plz_key.dta")%>% mutate(year=2019))
    data <- data %>% select(-plz) %>% left_join(plz_data)
  }
  return(data)
}