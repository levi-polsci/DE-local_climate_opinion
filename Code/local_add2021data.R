add2021data <- function (data){

  print("Adding 2021 data")
  new_data <- read_dta("Data/individual/SNB_2021_ak12_ak13_sozDem.dta") %>% mutate_if(is.numeric, as.numeric)
  data$e10_7 <- -1 # add freiflächen solar

  add_on <- data.frame(matrix(NA, nrow = nrow(new_data), ncol = ncol(data)))
  colnames(add_on) <- colnames(data)
  add_on <- add_on %>% mutate(gkz = new_data$gemkey) %>% mutate(year=2021) %>%
    mutate(ges = new_data$fges) %>% mutate(altNum = new_data$altNum) %>% mutate(so1 = new_data$so7) %>% mutate(so2 = new_data$so8) %>%
    mutate(e5_1 = new_data$ak12_1) %>% mutate(e5_2 = new_data$ak12_2) %>% mutate(e5_3 = new_data$ak12_6) %>%
    mutate(e5_4 = new_data$ak12_3) %>% mutate(e5_5 = new_data$ak12_4) %>% mutate(e5_6 = new_data$ak12_5) %>%
     mutate(e10_1 = new_data$ak13_1) %>% mutate(e10_2 = new_data$ak13_2) %>% mutate(e10_3 = new_data$ak13_3) %>%
    mutate(e10_6 = new_data$ak13_7) %>% mutate(e10_7 = new_data$ak13_2)


  data <- rbind(data, add_on)
  data[data$e10_7==-1, "e10_7"]<-NA

  return(data)

}