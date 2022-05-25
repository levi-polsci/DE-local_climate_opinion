addSocioDemoData <- function (level="landkreis", disaggregate_cities=FALSE){

# Want even more?
  # check out -> https://www.govdata.de/web/guest/suchen/-/searchresult/q//f/state%3A00%2C

if(disaggregate_cities) print("Disaggregating Berlin") else warning("Berlin and Hamburg are aggregated!")

### Rechtliche Einheiten nach Wirtschaftsabschnitten
#7_2016 2018 Rechtliche Einheiten nach Wirtschaftsabschnitten.xlsx
wirtschaftsabschnitte <- read.xlsx("Data/contexts/other - landkreise/7_2016 2018 Rechtliche Einheiten nach Wirtschaftsabschnitten.xlsx", startRow = 6)
colnames(wirtschaftsabschnitte) <- c("gkz", "gemeinde_name", "WirtschAbsGesamt", "WirtschAbsBergbau","WirtschAbsVerarbeitend", "WirtschAbsEnergie","WirtschAbsWasser", "WirtschAbsBau", "WirtschAbsHandel",
                                     "WirtschAbsVerkehr", "WirtschAbsGatro", "WirtschAbsInfo", "WirtschAbsFinanz", "WirtschAbsWohn", "WirtschAbsWissen", "WirtschAbsWirtschaftsservce",
                                     "WirtschAbsBildung", "WirtschAbsSozial", "WirtschAbsKultur", "WirtschAbsService")
# Assign years
wirtschaftsabschnitte$Year <-0
wirtschaftsabschnitte$id <- rownames(wirtschaftsabschnitte)
wirtschaftsabschnitte[as.numeric(wirtschaftsabschnitte$id)>as.numeric(wirtschaftsabschnitte[!is.na(wirtschaftsabschnitte$gkz) & wirtschaftsabschnitte$gkz=="2018","id"]) &
   as.numeric(wirtschaftsabschnitte$id)<as.numeric(wirtschaftsabschnitte[!is.na(wirtschaftsabschnitte$gkz) & wirtschaftsabschnitte$gkz=="2017","id"]) ,"Year"]<- 2018
wirtschaftsabschnitte[as.numeric(wirtschaftsabschnitte$id)>as.numeric(wirtschaftsabschnitte[!is.na(wirtschaftsabschnitte$gkz) & wirtschaftsabschnitte$gkz=="2017","id"]) &
   as.numeric(wirtschaftsabschnitte$id)<as.numeric(wirtschaftsabschnitte[!is.na(wirtschaftsabschnitte$gkz) & wirtschaftsabschnitte$gkz=="2016","id"]) ,"Year"]<- 2017
wirtschaftsabschnitte[as.numeric(wirtschaftsabschnitte$id)>as.numeric(wirtschaftsabschnitte[!is.na(wirtschaftsabschnitte$gkz) & wirtschaftsabschnitte$gkz=="2016","id"]),"Year"]<- 2016

wirtschaftsabschnitte <- wirtschaftsabschnitte %>%
  drop_na(gemeinde_name) %>%
  relocate(Year, .before=WirtschAbsGesamt) %>%
  select(-'id') %>%  select(-'gemeinde_name')


### Verkehr Kraftfahrzeugbestand
Kraftfahrzeugarten <- read.xlsx("Data/contexts/other - landkreise/1_2016 2020 Kraftfahrzeugbestand nach Kraftfahrzeugarten.xlsx", startRow = 5)
colnames(Kraftfahrzeugarten) <- c("gkz", "gemeinde_name", "KfzInsgesamt", "KfzPKW","KfzLkw", "KfzZugmaschinen","KfzKrafräder")
# Assign years
Kraftfahrzeugarten$Year <-0
Kraftfahrzeugarten$id <- rownames(Kraftfahrzeugarten)
Kraftfahrzeugarten[as.numeric(Kraftfahrzeugarten$id)>as.numeric(Kraftfahrzeugarten[!is.na(Kraftfahrzeugarten$gkz) & Kraftfahrzeugarten$gkz=="01.01.2020","id"]) &
                     as.numeric(Kraftfahrzeugarten$id)<as.numeric(Kraftfahrzeugarten[!is.na(Kraftfahrzeugarten$gkz) & Kraftfahrzeugarten$gkz=="01.01.2019","id"]) ,"Year"]<- 2020
Kraftfahrzeugarten[as.numeric(Kraftfahrzeugarten$id)>as.numeric(Kraftfahrzeugarten[!is.na(Kraftfahrzeugarten$gkz) & Kraftfahrzeugarten$gkz=="01.01.2019","id"]) &
                     as.numeric(Kraftfahrzeugarten$id)<as.numeric(Kraftfahrzeugarten[!is.na(Kraftfahrzeugarten$gkz) & Kraftfahrzeugarten$gkz=="01.01.2018","id"]) ,"Year"]<- 2019
Kraftfahrzeugarten[as.numeric(Kraftfahrzeugarten$id)>as.numeric(Kraftfahrzeugarten[!is.na(Kraftfahrzeugarten$gkz) & Kraftfahrzeugarten$gkz=="01.01.2018","id"]) &
                     as.numeric(Kraftfahrzeugarten$id)<as.numeric(Kraftfahrzeugarten[!is.na(Kraftfahrzeugarten$gkz) & Kraftfahrzeugarten$gkz=="01.01.2017","id"]) ,"Year"]<- 2018
Kraftfahrzeugarten[as.numeric(Kraftfahrzeugarten$id)>as.numeric(Kraftfahrzeugarten[!is.na(Kraftfahrzeugarten$gkz) & Kraftfahrzeugarten$gkz=="01.01.2017","id"]) &
                     as.numeric(Kraftfahrzeugarten$id)<as.numeric(Kraftfahrzeugarten[!is.na(Kraftfahrzeugarten$gkz) & Kraftfahrzeugarten$gkz=="01.01.2016","id"]) ,"Year"]<- 2017
Kraftfahrzeugarten[as.numeric(Kraftfahrzeugarten$id)>as.numeric(Kraftfahrzeugarten[!is.na(Kraftfahrzeugarten$gkz) & Kraftfahrzeugarten$gkz=="01.01.2016","id"]),"Year"]<- 2016


Kraftfahrzeugarten <- Kraftfahrzeugarten %>%
  drop_na(gemeinde_name) %>%
  relocate(Year, .before=KfzInsgesamt) %>%
  select(-'id') %>%  select(-'gemeinde_name')


### Verarbeitende Betriebe
VerarbGewerbe19 <- read.xlsx("Data/contexts/other - landkreise/2_2016_2019_3_Betriebe.xlsx", startRow = 4)
colnames(VerarbGewerbe19) <- c("gkz", "gemeinde_name", "Verarbeitende_Betriebe", "VerarbBetriebe_TätigePersonen","VerarbBetriebe_Bruttoentgelte")
# Assign years
VerarbGewerbe19$Year <-0
VerarbGewerbe19$id <- rownames(VerarbGewerbe19)

VerarbGewerbe19[as.numeric(VerarbGewerbe19$id)>as.numeric(VerarbGewerbe19[!is.na(VerarbGewerbe19$gkz) & VerarbGewerbe19$gkz=="30.09.2019","id"]) &
                     as.numeric(VerarbGewerbe19$id)<as.numeric(VerarbGewerbe19[!is.na(VerarbGewerbe19$gkz) & VerarbGewerbe19$gkz=="30.09.2018","id"]) ,"Year"]<- 2019
VerarbGewerbe19[as.numeric(VerarbGewerbe19$id)>as.numeric(VerarbGewerbe19[!is.na(VerarbGewerbe19$gkz) & VerarbGewerbe19$gkz=="30.09.2018","id"]) &
                     as.numeric(VerarbGewerbe19$id)<as.numeric(VerarbGewerbe19[!is.na(VerarbGewerbe19$gkz) & VerarbGewerbe19$gkz=="30.09.2017","id"]) ,"Year"]<- 2018
VerarbGewerbe19[as.numeric(VerarbGewerbe19$id)>as.numeric(VerarbGewerbe19[!is.na(VerarbGewerbe19$gkz) & VerarbGewerbe19$gkz=="30.09.2017","id"]) &
                     as.numeric(VerarbGewerbe19$id)<as.numeric(VerarbGewerbe19[!is.na(VerarbGewerbe19$gkz) & VerarbGewerbe19$gkz=="30.09.2016","id"]) ,"Year"]<- 2017
VerarbGewerbe19[as.numeric(VerarbGewerbe19$id)>as.numeric(VerarbGewerbe19[!is.na(VerarbGewerbe19$gkz) & VerarbGewerbe19$gkz=="30.09.2016","id"]),"Year"]<- 2016


VerarbGewerbe19 <- VerarbGewerbe19 %>%
  drop_na(gemeinde_name) %>%
  relocate(Year, .before=Verarbeitende_Betriebe) %>%
  select(-'id') %>%  select(-'gemeinde_name')

  ### Beschäftigungsdaten
  beschaeftigung <- readRDS("Data/contexts/other - landkreise/Beschaeftigungsdaten_Kreise/Besch_16_19.rds") %>% clean_names() %>% rename(Year=jahr)  %>% select(-"kommune")
  options(scipen = 999) # supress scientific notation
  beschaeftigung$gkz <- as.factor(ifelse(beschaeftigung$gkz < 10000000, paste0("0", as.character(beschaeftigung$gkz)),as.character(beschaeftigung$gkz)))
  beschaeftigung$gkz <- substr(beschaeftigung$gkz, 1,5) # use only the first five digits so it is aligned with the other data



### Landwirtschaftlich genutzte Fläche
  LandwFlaeche2020 <- read.xlsx("Data/contexts/other - landkreise/1_2016Landwirtschaftliche_Betriebe.xlsx", startRow =8) %>% select(-"Deutschland")
LandwFlaeche2016 <- read.xlsx("Data/contexts/other - landkreise/1_2016Landwirtschaftliche_Betriebe.xlsx", startRow =8) %>% select(-"Deutschland")
colnames(LandwFlaeche2020) <- c("gkz", "LandwirtschBetriebe", "LandwirtschFlächeHa","BodennutzungAckerland_Betriebe","BodennutzungAckerland_ha","BodennutzungDauerkulturen_Betriebe","BodennutzungDauerkulturen_ha", "BodennutzungDauergrün_Betriebe","BodennutzungDauergrün_ha" )
colnames(LandwFlaeche2016) <- c("gkz", "LandwirtschBetriebe", "LandwirtschFlächeHa","BodennutzungAckerland_Betriebe","BodennutzungAckerland_ha","BodennutzungDauerkulturen_Betriebe","BodennutzungDauerkulturen_ha", "BodennutzungDauergrün_Betriebe","BodennutzungDauergrün_ha" )
LandwFlaeche2020$Year <-2020 # Assign years
LandwFlaeche2016$Year <-2016


###Bodenfläche nach tatsächlicher Nutzung
BodenNutzung <- read.xlsx("Data/contexts/other - landkreise/1 Bodenflaaeche.xlsx", startRow =7)
# Rename columns
colnames(BodenNutzung) <- c("gkz", "gemeinde_name", "Bodenfl_Insg", "Bodenfl_Siedlungha","Bodenfl_Verkehrha", "BodenflVeg_Insg", "BodenflVeg_Landwirtschaft", "BodenflVeg_Wald", "BodenflVeg_Gehölz", "BodenflVeg_Heide", "BodenflVeg_Moor", "BodenflVeg_Sumpf", "BodenflVeg_Unland", "BodenflGew_Insg", "BodenflGew_Fließ", "BodenflGew_Hafen", "BodenflGew_Stehend", "BodenflGew_Meer")
# Assign years
BodenNutzung$Year <-0
BodenNutzung$id <- rownames(BodenNutzung)
BodenNutzung[as.numeric(BodenNutzung$id)>as.numeric(BodenNutzung[!is.na(BodenNutzung$gkz) & BodenNutzung$gkz=="31.12.2020","id"]) &
               as.numeric(BodenNutzung$id)<as.numeric(BodenNutzung[!is.na(BodenNutzung$gkz) & BodenNutzung$gkz=="31.12.2019","id"]) ,"Year"]<- 2020
BodenNutzung[as.numeric(BodenNutzung$id)>as.numeric(BodenNutzung[!is.na(BodenNutzung$gkz) & BodenNutzung$gkz=="31.12.2019","id"]) &
                  as.numeric(BodenNutzung$id)<as.numeric(BodenNutzung[!is.na(BodenNutzung$gkz) & BodenNutzung$gkz=="31.12.2018","id"]) ,"Year"]<- 2019
BodenNutzung[as.numeric(BodenNutzung$id)>as.numeric(BodenNutzung[!is.na(BodenNutzung$gkz) & BodenNutzung$gkz=="31.12.2018","id"]) &
                  as.numeric(BodenNutzung$id)<as.numeric(BodenNutzung[!is.na(BodenNutzung$gkz) & BodenNutzung$gkz=="31.12.2017","id"]) ,"Year"]<- 2018
BodenNutzung[as.numeric(BodenNutzung$id)>as.numeric(BodenNutzung[!is.na(BodenNutzung$gkz) & BodenNutzung$gkz=="31.12.2017","id"]) &
                  as.numeric(BodenNutzung$id)<as.numeric(BodenNutzung[!is.na(BodenNutzung$gkz) & BodenNutzung$gkz=="31.12.2016","id"]) ,"Year"]<- 2017
BodenNutzung[as.numeric(BodenNutzung$id)>as.numeric(BodenNutzung[!is.na(BodenNutzung$gkz) & BodenNutzung$gkz=="31.12.2016","id"]),"Year"]<- 2016


BodenNutzung <- BodenNutzung %>%
  drop_na(gemeinde_name) %>%
  relocate(Year, .before=Bodenfl_Insg) %>%
  select(-'id') %>%select(-'gemeinde_name')


###Daten left join
socioDemoData <- join_all(list(Kraftfahrzeugarten, wirtschaftsabschnitte, VerarbGewerbe19, beschaeftigung, LandwFlaeche2020, LandwFlaeche2016, BodenNutzung),
                         by=c("gkz", "Year"),
                         type = "left") %>% clean_names()

  # aggregate context to years and gemeinde to cross-sectional Landkreise (maybe leave refined at a later point of time)
socioDemoData[socioDemoData$gkz=="02", "gkz"]<- "02000"
  if(!disaggregate_cities) socioDemoData[socioDemoData$gkz=="11", "gkz"]<- "11000" else{
    suppressWarnings(cond_berlin <- (!is.na(as.numeric(socioDemoData$gkz)) & as.numeric(socioDemoData$gkz)>11000000 & as.numeric(socioDemoData$gkz)<12000000))
    socioDemoData[cond_berlin, "gkz"] <- substr(as.character( socioDemoData[cond_berlin, "gkz"] ),1,5)
}

suppressWarnings(socioDemoData <- socioDemoData[!is.na(as.numeric(as.character(socioDemoData$gkz))) & (as.numeric(as.character(socioDemoData$gkz))>100),] %>%
  collap(~ gkz + year)  %>%
  mutate(across(gkz, as.factor)) %>%
  mutate(across(where(is.character), ~na_if(., "-"))) %>% # - exchange "-" and "." for NA
  mutate(across(where(is.character), ~na_if(., "."))) %>%
  mutate(across(where(is.character), as.numeric)) %>% # # - convert to numbers
  mutate(across(gkz, as.character)) %>% rename(lk_kz=gkz))

  return(socioDemoData)
}