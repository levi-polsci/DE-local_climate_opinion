getOtherKommuneData <- function (){

  ##### Get RWI kommune-level data
  # Here you have: population; nr. households; purchuase power; nr_cars; nr_EV; unemployment rate
  data <- readRDS("Data/contexts/other - kommune/microm_gem.RDS") %>%
    select(ags, nr_household ,population, purchase_power, nr_cars, unemployment_rate) %>% rename(gkz_f = ags)

  ##### Attach and weight city level
  # Load and preprocess
  city_B <- read.xlsx("Data/contexts/other - cities/Bezirke ALG .xlsx", sheet = "Berlin_2", startRow = 2) %>%
    clean_names() %>% select(gkz, haushalte, bevolkerung_insg,  durch_haushalts_nettoeinkommen, pkw, sgb_ii_und_iii_percent) %>%
    rename(gkz_f = gkz, nr_household=haushalte, population = bevolkerung_insg, purchase_power=durch_haushalts_nettoeinkommen,
           nr_cars=pkw, unemployment_rate=sgb_ii_und_iii_percent) %>% mutate(purchase_power = purchase_power * nr_household)
    city_B$gkz_f <- as.factor(city_B$gkz_f)

  city_H <- read.xlsx("Data/contexts/other - cities/Bezirke ALG .xlsx", sheet = "Hamburg", startRow = 2) %>%
    clean_names() %>% filter(year==2016) %>% select(gkz, haushalte, bevolkerung_insg, einkunfte, pkw, sgb_ii_percent) %>%
    rename(gkz_f = gkz, nr_household=haushalte, population = bevolkerung_insg, purchase_power=einkunfte,
           nr_cars=pkw, unemployment_rate=sgb_ii_percent) %>%
    mutate(unemployment_rate=unemployment_rate*100) %>% mutate(purchase_power = purchase_power * nr_household)
    options(scipen = 999) # supress scientific notation
  city_H$population <- as.numeric(city_H$population)
  city_H$gkz_f <- as.factor(paste0("0", as.character(city_H$gkz_f),"000"))

  city_M <- read.xlsx("Data/contexts/other - cities/Bezirke ALG .xlsx", sheet = "Munchen", startRow = 2) %>%
    clean_names() %>% filter(year==2016) %>% select(gkz, haushalte_insg, bevolkerung_i, pro_kopf_einkommen, pkw, sgb_ii_iii_percent) %>%
    rename(gkz_f = gkz, nr_household=haushalte_insg, population = bevolkerung_i, purchase_power=pro_kopf_einkommen,
           nr_cars=pkw, unemployment_rate=sgb_ii_iii_percent) %>%
    mutate(unemployment_rate=unemployment_rate*100) %>% mutate(purchase_power = purchase_power * population)
  city_M$gkz_f <- as.factor(paste0("0", as.character(city_M$gkz_f)))

  #### Adjust purchuase power and potentially
  sum(city_B$purchase_power, na.rm = TRUE)
  data[data$gkz_f=="11000000", "purchase_power"]
  # BERLIN
  c <- as.numeric(data[data$gkz_f=="11000000", "unemployment_rate"])/weighted.mean(city_B$unemployment_rate, na.rm = TRUE, w=city_B$population)
  for (i in seq_len(nrow(city_B))) city_B[i, "unemployment_rate"] <- city_B[i, "unemployment_rate"] * c
  c <- as.numeric(data[data$gkz_f=="11000000", "purchase_power"])/sum(city_B$purchase_power, na.rm = TRUE)
  for (i in seq_len(nrow(city_B))) city_B[i, "purchase_power"] <- city_B[i, "purchase_power"] * c
  # H
  c <- as.numeric(data[data$gkz_f=="02000000", "unemployment_rate"])/ weighted.mean(city_H$unemployment_rate, na.rm = TRUE, w=city_H$population)
  for (i in seq_len(nrow(city_H))) city_H[i, "unemployment_rate"] <- city_H[i, "unemployment_rate"] * c
  c <- as.numeric(data[data$gkz_f=="02000000", "purchase_power"])/sum(city_H$purchase_power, na.rm = TRUE)
  for (i in seq_len(nrow(city_H))) city_H[i, "purchase_power"] <- city_H[i, "purchase_power"] * c
  # M
  c <- as.numeric(data[data$gkz_f=="02000000", "unemployment_rate"])/ weighted.mean(city_H$unemployment_rate, na.rm = TRUE, w=city_H$population)
  for (i in seq_len(nrow(city_H))) city_H[i, "unemployment_rate"] <- city_H[i, "unemployment_rate"] * c
  c <- as.numeric(data[data$gkz_f=="02000000", "purchase_power"])/sum(city_H$purchase_power, na.rm = TRUE)
  for (i in seq_len(nrow(city_H))) city_H[i, "purchase_power"] <- city_H[i, "purchase_power"] * c

  # M
  c <- as.numeric(data[data$gkz_f=="09162000", "unemployment_rate"])/weighted.mean(city_M$unemployment_rate, na.rm = TRUE, w=city_M$population)
  for (i in seq_len(nrow(city_M))) city_M[i, "unemployment_rate"] <- city_M[i, "unemployment_rate"] * c
  c <- as.numeric(data[data$gkz_f=="09162000", "purchase_power"])/sum(city_M$purchase_power, na.rm = TRUE)
  for (i in seq_len(nrow(city_M))) city_M[i, "purchase_power"] <- city_M[i, "purchase_power"] * c

  # Merge
  data_combined <- rbind(data,city_B, city_M, city_H)
  return(data_combined)
}