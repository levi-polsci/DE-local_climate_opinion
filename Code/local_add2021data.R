add2021data <- function (data){

  #summary(as.factor(fsolar$e10_2))
  # Adding Freiflächensolar to 2017-2019
  fsolar <- rbind(read_dta("Data/individual/SNB_2018_PV-Freiflaeche.dta") %>% mutate(year=2018) ,
                  read_dta("Data/individual/SNB_2019_PV-Freiflaeche.dta") %>% mutate(year=2019)) %>%
    dplyr::rename(e10_7 = e10_2) %>% select(key, year, e10_7)
  data <- left_join(data, fsolar)


  print("Adding 2021 data")
  new_data <- read_dta("Data/individual/SNB_2021_ak12_ak13_sozDem.dta") %>% mutate_if(is.numeric, as.numeric)
  new_data_transport <- read_dta("Data/individual/SNB_2021_transport measures.dta")  %>% mutate_if(is.numeric, as.numeric)
  new_data_all <- read_dta("Data/individual/Barometer_2021_Levi_100821.dta") %>% mutate_if(is.numeric, as.numeric) %>%
    select(id, ak15_3) %>% mutate(across(ak15_3, car::recode, '1=1;2=1;4=2;5=2;6=3')) # 1 bin bereits 2 bin nicht bereit 3 idk
  summary(as.factor(new_data_all$ak15_3))

  ids <- sample(1:6822, 20)
  new_data[ids, "gemkey"]
  new_data_transport[ids, "gemkey"]

  new_data_transport[ids, "id"]
  new_data_all[ids, "id"]

  data[is.na(data$u3d), "u3d"] <- data[is.na(data$u3d), "u3d_1"] # merge data on Verbrennungsmotor in one variable
  data$u3d_1 <- data$u3d

  add_on <- data.frame(matrix(NA, nrow = nrow(new_data), ncol = ncol(data)))
  colnames(add_on) <- colnames(data)
  add_on <- add_on %>% mutate(gkz = new_data$gemkey) %>% mutate(plz = new_data_transport$plz) %>%
    mutate(year=2021) %>%  mutate(key = new_data_transport$id) %>%
    mutate(ges = new_data$fges) %>% mutate(altNum = new_data$altNum) %>% mutate(so1 = new_data$so7) %>% mutate(so2 = new_data$so8) %>%
    mutate(e5_1 = new_data$ak12_1) %>% mutate(e5_2 = new_data$ak12_2) %>% mutate(e5_3 = new_data$ak12_6) %>%
    mutate(e5_4 = new_data$ak12_3) %>% mutate(e5_5 = new_data$ak12_4) %>% mutate(e5_6 = new_data$ak12_5) %>%
     mutate(e10_1 = new_data$ak13_1) %>% mutate(e10_2 = new_data$ak13_2) %>% mutate(e10_3 = new_data$ak13_3) %>%
    mutate(e10_6 = new_data$ak13_7) %>% mutate(e10_7 = new_data$ak13_2) %>%
    mutate(u3d_1 = new_data_transport$ak20a_1 ) %>% mutate(u3d_2 = new_data_transport$ak20_7 ) %>%
    mutate(u3d_4 = new_data_transport$ak20_11) %>% mutate(u3d_6 = new_data_transport$ak20_10) %>%
    mutate(u3d_7 = new_data_transport$ak20_8) %>% mutate(u3d_8 = new_data_transport$ak20_5)  %>%
    mutate(c0 = new_data_all$ak15_3)

  data <- rbind(data, add_on)
  #data[data$e10_7==-1, "e10_7"]<-NA

  return(data)

}