getHeatData <- function (level="landkreis", disaggregate_cities=FALSE){

  # Add Waermedaten 1
  suppressWarnings( waerme1 <- read.xlsx("Data/contexts/other - landkreise/Waermedaten Kreis Ebene/Waermedaten_Kreisebene.xlsx") %>% clean_names() %>%
    select(gkz, year, fert_wohnungen_ee, wohnflaeche_p_p, flaecheninanspruchnahme, flaechennutzung_ha_p_p) %>% filter(year==2019) %>% select(-year) %>%
    mutate(across(all_of(c('fert_wohnungen_ee', 'wohnflaeche_p_p', 'flaecheninanspruchnahme', 'flaechennutzung_ha_p_p')), as.numeric)))

  options(scipen = 999) # supress scientific notation
  waerme1$gkz <- as.factor(ifelse(waerme1$gkz < 10000000, paste0("0", as.character(waerme1$gkz)),as.character(waerme1$gkz)))
  waerme1$gkz <- substr(waerme1$gkz, 1,5) # use only the first five digits so it is aligned with the other data

  # Add Heizungsdaten
  suppressWarnings(waerme2 <- read.xlsx("Data/contexts/other - landkreise/Waermedaten Kreis Ebene/Heizung nach Typ Zensus Kreis Ebene.xlsx") %>% clean_names() %>% select(-kommune) %>%
    dplyr::rename(heizungen = insgesamt) %>% mutate(across(all_of(c('fernheizung_fernwarme', 'etagenheizung', 'blockheizung', 'zentralheizung', 'einzel_oder_mehrraumofen', 'keine_heizung_im_gebaude')), as.numeric)))

    # aggregate context to years and gemeinde to cross-sectional Landkreise (maybe leave refined at a later point of time)
waerme2[waerme2$gkz=="02", "gkz"]<- "02000"
  waerme2[waerme2$gkz=="11", "gkz"]<- "11000"


  # merge and return
  waermedaten <- left_join(waerme1, waerme2, by="gkz") %>% mutate(across(gkz, as.character)) %>% dplyr::rename(lk_kz=gkz)

  return(waermedaten)


}