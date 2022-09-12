addAreaCode <- function (data, disaggregate_cities=FALSE){

survey_clean <- data
  print("Adding location, census, and outcome variables ...")

  options(scipen = 999) # supress scientific notation
  survey_clean$gkz_f <- as.factor(ifelse(survey_clean$gkz < 10000000, paste0("0", as.character(survey_clean$gkz)),as.character(survey_clean$gkz)))
  survey_clean$lk_kz <- as.factor(substr(as.character(survey_clean$gkz_f), 1,5))
  survey_clean$bl_kz <- as.factor(substr(as.character(survey_clean$gkz_f), 1,2))

  # Add Gemeindenamen
  gemeinde_codebook <- openxlsx::read.xlsx("Data/areas/AuszugGV3QAktuell.xlsx", sheet = 2, startRow = 4)
  colnames(gemeinde_codebook)[3:8] <- c("ARSLand", "ARSRB", "ARSKreis", "ARSVB", "ARSGem", "Gemeindename")
  gemeinde_codebook <- gemeinde_codebook[3:nrow(gemeinde_codebook),1:8]
  gem <- gemeinde_codebook %>%
    drop_na(ARSGem) %>%
    mutate(gkz_f = paste0(ARSLand, ARSRB, ARSKreis, ARSGem))
  survey_clean <- left_join(survey_clean,
                            gem[,c("gkz_f", "Gemeindename")], by="gkz_f")

  # Add Kreisname
  kreise <- gemeinde_codebook %>%
    drop_na(ARSKreis) %>%
    filter(is.na(ARSGem)) %>%
    mutate(lk_kz = paste0(ARSLand, ARSRB, ARSKreis)) %>%
    distinct(lk_kz, .keep_all = TRUE) %>%
    dplyr::rename(Landkreisname = Gemeindename)

  survey_clean <- left_join(survey_clean,
                            kreise[,c("lk_kz", "Landkreisname")], by="lk_kz")

  # Add BL Name
  bundeslaender <- gemeinde_codebook %>%
    filter(is.na(ARSRB)) %>%
    distinct(ARSLand, .keep_all = TRUE) %>%
     dplyr::rename(Bundesland = Gemeindename) %>%
     dplyr::rename(bl_kz = ARSLand) %>%
    drop_na(Bundesland)
  survey_clean <- left_join(survey_clean,
                            bundeslaender[,c("bl_kz", "Bundesland")], by="bl_kz")

  # Adress NAs
  #View(survey_clean[is.na(survey_clean$Gemeindename),c("gkz_f", "Gemeindename", "Landkreisname", "Bundesland")])
  #View(survey_clean[is.na(survey_clean$Landkreisname),c("gkz_f", "Gemeindename", "Landkreisname", "Bundesland")])
  survey_clean[survey_clean$gkz_f=="16056000", c("Landkreisname", "Gemeindename")]<- "Eisenach"
  #View(survey_clean[is.na(survey_clean$Bundesland),c("gkz_f", "Gemeindename", "Landkreisname", "Bundesland")])

  # Add WK-2017
  wk2017 <- openxlsx::read.xlsx("Data/contexts/votes/BTW 17/BTW 17 Wahlkreis Gemeinde Zuordnung/20170228_BTW17_WKr_Gemeinden_ASCII.xlsx", startRow = 8)
  wk2017$RGS_Land_f <- as.factor(ifelse(wk2017$RGS_Land < 10, paste0("0", as.character(wk2017$RGS_Land)),as.character(wk2017$RGS_Land)))
  wk2017$RGS_RegBez_f <- as.factor(as.character(wk2017$RGS_RegBez))
  wk2017$RGS_Kreis_f <- as.factor(ifelse(wk2017$RGS_Kreis < 10, paste0("0", as.character(wk2017$RGS_Kreis)),as.character(wk2017$RGS_Kreis)))
  wk2017$RGS_Gemeinde_f <- as.factor(ifelse(wk2017$RGS_Gemeinde < 10, paste0("00", as.character(wk2017$RGS_Gemeinde)),
                                            ifelse(wk2017$RGS_Gemeinde < 100, paste0("0", as.character(wk2017$RGS_Gemeinde)),as.character(wk2017$RGS_Gemeinde))))

  colnames(wk2017)[1:2] <- c("WK2017Nr", "WK2017Name")
  wk2017 <- wk2017 %>%
    mutate(gkz_f=paste0(RGS_Land_f,RGS_RegBez_f,RGS_Kreis_f,RGS_Gemeinde_f)) %>%
    distinct(gkz_f, .keep_all = TRUE)
  survey_clean <- left_join(survey_clean,
                            wk2017[,c("gkz_f", "WK2017Nr", "WK2017Name")], by="gkz_f")

  #View(survey_clean[is.na(survey_clean$WK2017Nr),c("gkz_f", "Gemeindename", "Landkreisname", "Bundesland")])
  # TODO: In which Wahlkreis is Helmstedt, Stadt?

  # Add WK-2021
  wk2021 <- openxlsx::read.xlsx("Data/contexts/votes/BTW 21/BTW 21 Wahlkreis-Gemeinde Zuordnung/20200415_btw21_wkr_gemeinden_utf8.xlsx", startRow = 8)
  wk2021$RGS_Land_f <- as.factor(ifelse(wk2021$RGS_Land < 10, paste0("0", as.character(wk2021$RGS_Land)),as.character(wk2021$RGS_Land)))
  wk2021$RGS_RegBez_f <- as.factor(as.character(wk2021$RGS_RegBez))
  wk2021$RGS_Kreis_f <- as.factor(ifelse(wk2021$RGS_Kreis < 10, paste0("0", as.character(wk2017$RGS_Kreis)),as.character(wk2021$RGS_Kreis)))
  wk2021$RGS_Gemeinde_f <- as.factor(ifelse(wk2021$RGS_Gemeinde < 10, paste0("00", as.character(wk2021$RGS_Gemeinde)),
                                            ifelse(wk2021$RGS_Gemeinde < 100, paste0("0", as.character(wk2021$RGS_Gemeinde)),as.character(wk2021$RGS_Gemeinde))))

  colnames(wk2021)[1:2] <- c("WK2021Nr", "WK2021Name")
  wk2021 <- wk2021 %>%
    mutate(gkz_f=paste0(RGS_Land_f,RGS_RegBez_f,RGS_Kreis_f,RGS_Gemeinde_f)) %>%
    distinct(gkz_f, .keep_all = TRUE)
  survey_clean <- left_join(survey_clean,
                            wk2021[,c("gkz_f", "WK2021Nr", "WK2021Name")], by="gkz_f")

  nrow(survey_clean[is.na(survey_clean$WK2021Nr),c("gkz_f", "Gemeindename", "Landkreisname", "Bundesland")])
  #View(survey_clean[is.na(survey_clean$WK2021Nr),c("gkz_f", "Gemeindename", "Landkreisname", "Bundesland")])
  # TODO: 2417 WK missing - mostly cities

  if(disaggregate_cities) {
    print("Disaggregating districts for Berlin.")

    city_districts <- read.xlsx("Data/areas/Berlin PLZ nach Bezirken_v2.xlsx")

    options(scipen = 999) # supress scientific notation
    city_districts$gkz_f <- as.factor(ifelse(city_districts$GKZ < 10000000, paste0("0", as.character(city_districts$GKZ)),as.character(city_districts$GKZ)))
    city_districts$plz <- as.character(city_districts$PLZ)

    #View(survey_clean[survey_clean$bl_kz==11 & !is.na(survey_clean$plz),c("plz","gkz_f", "Gemeindename")])
    survey_clean$gkz_f <- as.character(survey_clean$gkz_f)
    survey_clean$plz <- as.character(survey_clean$plz)
    survey_clean <- as.data.frame(survey_clean)
    survey_clean <- survey_clean[!(survey_clean$bl_kz==11 & !(survey_clean$plz %in%  city_districts$PLZ)),]

    # Convert and assign
    plzs <- survey_clean[survey_clean$bl_kz==11 & !is.na(survey_clean$plz), "plz"]
    gkzs <- unlist(lapply(plzs,  function (x) as.character(city_districts[city_districts$PLZ==x, "gkz_f"])))
    name <- unlist(lapply(plzs,  function (x) paste0("Berlin, ", as.character(city_districts[city_districts$PLZ==x, "Bezirk"]))))
    survey_clean[survey_clean$bl_kz==11 & !is.na(survey_clean$plz), "gkz_f"] <-gkzs
    survey_clean[survey_clean$bl_kz==11 & !is.na(survey_clean$plz), "Gemeindename"] <-name


    print("Disaggregating districts for Hamburg")
    city_districts_HH <- read.xlsx("Data/areas/city_districts.xlsx", sheet = 2)
    options(scipen = 999) # supress scientific notation
    city_districts_HH$gkz_f <- as.factor(unlist(lapply(city_districts_HH$gkz,  function (x) paste0(as.character(x), "000"))))
    city_districts_HH$plz <- as.character(city_districts_HH$PLZ)

    survey_clean$gkz_f <- as.character(survey_clean$gkz_f)
    survey_clean$plz <- as.character(survey_clean$plz)
    survey_clean <- as.data.frame(survey_clean)
    survey_clean <- survey_clean[!(survey_clean$bl_kz=="02" & !(survey_clean$plz %in%  city_districts_HH$plz)),]

    # Convert and assign
    plzs <- survey_clean[survey_clean$bl_kz=="02" & !is.na(survey_clean$plz), "plz"]
    gkzs <- unlist(lapply(plzs,  function (x) as.character(city_districts_HH[city_districts_HH$plz==x, "gkz_f"][1])))
    name <- unlist(lapply(plzs,  function (x) as.character(city_districts_HH[city_districts_HH$PLZ==x, "Bezirk"][1])))
    survey_clean[survey_clean$bl_kz=="02" & !is.na(survey_clean$plz), "gkz_f"] <-  gkzs
    survey_clean[survey_clean$bl_kz=="02" & !is.na(survey_clean$plz), "Gemeindename"] <- name



    print("Disaggregating districts for München - in process")
    city_districts <- read.xlsx("Data/areas/city_districts.xlsx", sheet = 4)
    options(scipen = 999) # supress scientific notation
    city_districts$gkz_f <- as.factor(ifelse(city_districts$GKZ < 10000000, paste0("0", as.character(city_districts$GKZ)),as.character(city_districts$GKZ)))
    city_districts$plz <- as.character(city_districts$PLZ)

    survey_clean$gkz_f <- as.character(survey_clean$gkz_f)
    survey_clean$plz <- as.character(survey_clean$plz)
    survey_clean <- as.data.frame(survey_clean)
    survey_clean <- survey_clean[!(survey_clean$lk_kz=="09162" & !(survey_clean$plz %in%  city_districts$plz)),]

    # Convert and assign
    plzs <- survey_clean[survey_clean$lk_kz=="09162" & !is.na(survey_clean$plz), "plz"]
    gkzs <- unlist(lapply(plzs,  function (x) as.character(city_districts[city_districts$plz==x, "gkz_f"][1])))
    name <- unlist(lapply(plzs,  function (x) paste0("München, ", as.character(city_districts[city_districts$plz==x, "Bezirk"][1]))))
    survey_clean[survey_clean$lk_kz=="09162" & !is.na(survey_clean$plz), "gkz_f"] <-  gkzs
    survey_clean[survey_clean$lk_kz=="09162" & !is.na(survey_clean$plz), "Gemeindename"] <- name



  }

  return(survey_clean)


}