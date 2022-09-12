getElectionData <- function (level, disaggregate_cities){

  if(!(level %in% c("landkreis", "kommune")))
    stop("Please specify level as either 'landkreis' or 'kommune'")

  if(level=="landkreis"){
    erststimmen <- read.csv2("Data/contexts/votes/BTW 17/BTW 17 Ergebnisse/BTW 2017 Ergebnisse - Wahlkreise/btw17_wbz_erststimmen.csv", skip=4) %>% clean_names() %>% addGKZ_f()
    erststimmen[erststimmen$RGS_Land_f=="11","RGS_RegBez_f"] <- "0"
    if(!disaggregate_cities)  erststimmen[erststimmen$RGS_Land_f=="11","RGS_Kreis_f"] <- "00"


    erststimmen_agg <- erststimmen %>%
      mutate(gkz_f=paste0(RGS_Land_f,RGS_RegBez_f,RGS_Kreis_f,RGS_Gemeinde_f)) %>%
      mutate(kkz_f=paste0(RGS_Land_f,RGS_RegBez_f,RGS_Kreis_f)) %>%
      select(kkz_f, wahlberechtigte = wahlberechtigte_ohne_sperrvermerk_a1, unguelt1 = ungultige,
            cdu1 = cdu, spd1=spd, linke1=die_linke, gruene1= grune, csu1=csu, fdp1=fdp, afd1=af_d, piraten1= piraten,
            npd1=npd, frwae1=freie_wahler,tiersch1=tierschutzpartei, partei1=die_partei) %>%
      collap(~kkz_f, FUN=fsum) %>%
      mutate(across(unguelt1:partei1, ~ .x/wahlberechtigte *100))

    zweitstimmen <- read.csv2("Data/contexts/votes/BTW 17/BTW 17 Ergebnisse/BTW 2017 Ergebnisse - Wahlkreise/btw17_wbz_zweitstimmen.csv", skip=4)%>% clean_names() %>% addGKZ_f()
    zweitstimmen[zweitstimmen$RGS_Land_f=="11","RGS_RegBez_f"] <- "0"
    if(!disaggregate_cities) zweitstimmen[zweitstimmen$RGS_Land_f=="11","RGS_Kreis_f"] <- "00"

    zweitstimmen_agg <- zweitstimmen %>%
      mutate(gkz_f=paste0(RGS_Land_f,RGS_RegBez_f,RGS_Kreis_f,RGS_Gemeinde_f)) %>%
      mutate(kkz_f=paste0(RGS_Land_f,RGS_RegBez_f,RGS_Kreis_f)) %>%
      select(kkz_f, wahlberechtigte = wahlberechtigte_ohne_sperrvermerk_a1, unguelt2 = ungultige,
             cdu2 = cdu, spd2=spd, linke2=die_linke, gruene2= grune, csu2=csu, fdp2=fdp, afd2=af_d, piraten2= piraten,
            npd2=npd, frwae2=freie_wahler,tiersch2=tierschutzpartei, partei2=die_partei) %>%
      collap(~kkz_f, FUN=fsum) %>%
      mutate(across(unguelt2:partei2, ~ .x/wahlberechtigte *100)) %>%
      select(-wahlberechtigte)

    data <- left_join(erststimmen_agg, zweitstimmen_agg, by="kkz_f") %>% dplyr::rename(lk_kz=kkz_f)
  }

  # check which you have "too much"
  #leitband <- read.csv2("Data/btw17_wbz/btw17_wbz_leitband.csv", skip=4)%>%  clean_names() %>% addGKZ_f() %>%
  #mutate(gkz_f=paste0(RGS_Land_f,RGS_RegBez_f,RGS_Kreis_f,RGS_Gemeinde_f)) %>%
  #  mutate(kkz_f=paste0(RGS_Land_f,RGS_RegBez_f,RGS_Kreis_f)) %>% select(name, gkz_f, kkz_f)

  # Das sind die Berliner Bezirke!
 # View(leitband[leitband$kkz_f %in% election_data[!(election_data$kkz_f %in% SNB_clean$lk_kz), "kkz_f"], ])

  if(level=="kommune"){
    zweitstimmen <- read.xlsx("Data/contexts/votes/BTW 17/BTW 17 Ergebnisse/BTW 2017 Ergebnisse - Gemeinden/btw17_wbz_zweitstimmen.xlsx", startRow = 5) %>%
      clean_names() %>% addGKZ_f()

    # drop München and Berlin
    zweitstimmen <- zweitstimmen[zweitstimmen$RGS_Land_f!="02" & zweitstimmen$RGS_Land_f!="11",]

    zweitstimmen_agg <- zweitstimmen %>%
      mutate(gkz_f=paste0(RGS_Land_f,RGS_RegBez_f,RGS_Kreis_f,RGS_Gemeinde_f)) %>%
      mutate(kkz_f=paste0(RGS_Land_f,RGS_RegBez_f,RGS_Kreis_f)) %>%
      select(gkz_f, wahlberechtigte = wahlberechtigte_ohne_sperrvermerk_a1,  gultige,unguelt2 = ungultige,
             cdu2 = cdu, spd2=spd, linke2=die_linke, gruene2= grune, csu2=csu, fdp2=fdp, afd2=af_d, piraten2= piraten,
            npd2=npd, frwae2=freie_wahler,tiersch2=tierschutzpartei, partei2=die_partei) %>%
      collap(~gkz_f, FUN=fsum) %>%
      mutate(across(unguelt2:partei2, ~ .x/wahlberechtigte *100)) %>%
      select(-wahlberechtigte)

    # Drop München
    zweitstimmen_agg <- zweitstimmen_agg[zweitstimmen_agg$gkz_f !="09162000",]
    #Add Berlin, Hamburg, München
    zweitstimmen_cities <- read.xlsx("Data/contexts/votes/BTW 17/BTW 17 Ergebnisse/BTW 2017 Ergebnisse - Gemeinden/BTW 17 Bezirke B_HH_M.xlsx") %>% clean_names()
    zweitstimmen_cities[is.na(zweitstimmen_cities)]<-0
    for (i in seq_len(nrow(zweitstimmen_cities))){
      zweitstimmen_agg <- rbind(zweitstimmen_agg,
                                  data.frame("gkz_f"=zweitstimmen_cities[i, "gkz"], "gultige"=0,"unguelt2"=0,
                                             "cdu2"=zweitstimmen_cities[i, "cdu"], "spd2"=zweitstimmen_cities[i, "spd"], "linke2"=zweitstimmen_cities[i, "die_linke"],
                                             "gruene2"=zweitstimmen_cities[i, "grune"],"csu2"=zweitstimmen_cities[i, "csu"], "fdp2"=zweitstimmen_cities[i, "fdp"],
                                             "afd2"=zweitstimmen_cities[i, "afd"],"piraten2"=0, "npd2"=0, "frwae2"=0, "tiersch2"=0, "partei2"=0)
      )
    }
    data <- zweitstimmen_agg

  }


  return(data)
}