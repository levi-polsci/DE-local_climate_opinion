### LK data
name <- readRDS("Data/results/results_lk_20220720.rds") %>% select(ags, Landkreisname) %>% rename(gkz = ags) %>% collap(Landkreisname ~ gkz)

data_LK <- readRDS("Data/contexts/other - landkreise/Context 2017-2019+2021/Kontextdaten2016-2019+2021.rds") %>%
  clean_names() %>% dplyr::rename(solar ="solare_strahlungsenergie") %>% mutate(kohle = braunkohle + steinkohle) %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))  %>% left_join(name) %>%
  mutate(einheit = case_when(grepl("Stadt", Landkreisname) | grepl("stadt", Landkreisname) ~ "Stadt",
                             TRUE ~ "Landkreis")) %>%
  select(gkz, year, bev, kohle, wind, solar,af_d, grune,spd, cdu_csu,die_linke,fdp, gultige_zweitstimmen, einheit) %>%
  mutate(across(bev:gultige_zweitstimmen, as.numeric))

#Göttingen Landkreis
data_LK[is.na(data_LK$grune) & !is.na(data_LK$fdp), "grune"] <- 0
data_LK[data_LK$gkz=="03152", "bev"] <- 330000
data_LK[data_LK$gkz=="03152" & data_LK$year==2021, "spd"] <- 51180
data_LK[data_LK$gkz=="03152" & data_LK$year==2021, "cdu_csu"] <- 34588
data_LK[data_LK$gkz=="03152"& data_LK$year==2021, "grune"] <- 32448
data_LK[data_LK$gkz=="03152"& data_LK$year==2021, "fdp"] <- 15638
data_LK[data_LK$gkz=="03152"& data_LK$year==2021, "af_d"] <- 9785
data_LK[data_LK$gkz=="03152"& data_LK$year==2021, "die_linke"] <- 0 # probbly wrong, but doesnt matter at this point
data_LK[data_LK$gkz=="03152"& data_LK$year==2021, "gultige_zweitstimmen"] <- 160305

data_LK[data_LK$gkz=="03156", "bev"] <- 30000
data_LK[data_LK$gkz=="03156" & data_LK$year==2021, "spd"] <- 4841
data_LK[data_LK$gkz=="03156" & data_LK$year==2021, "cdu_csu"] <- 2998
data_LK[data_LK$gkz=="03156"& data_LK$year==2021, "grune"] <- 1399
data_LK[data_LK$gkz=="03156"& data_LK$year==2021, "fdp"] <- 1214
data_LK[data_LK$gkz=="03156"& data_LK$year==2021, "af_d"] <- 923
data_LK[data_LK$gkz=="03156"& data_LK$year==2021, "die_linke"] <- 0 # probbly wrong, but doesnt matter at this point
data_LK[data_LK$gkz=="03156"& data_LK$year==2021, "gultige_zweitstimmen"] <- 12212

data_LK[data_LK$gkz=="16056" & data_LK$year==2021, "spd"] <- 38363
data_LK[data_LK$gkz=="16056" & data_LK$year==2021, "cdu_csu"] <- 28249
data_LK[data_LK$gkz=="16056"& data_LK$year==2021, "grune"] <- 7469
data_LK[data_LK$gkz=="16056"& data_LK$year==2021, "fdp"] <- 13258
data_LK[data_LK$gkz=="16056"& data_LK$year==2021, "af_d"] <- 37737
data_LK[data_LK$gkz=="16056"& data_LK$year==2021, "die_linke"] <- 0 # probbly wrong, but doesnt matter at this point
data_LK[data_LK$gkz=="16056"& data_LK$year==2021, "gultige_zweitstimmen"] <- 154221

data_BL <- data_LK %>% mutate(gkz = substr(gkz, 1,2)) %>% collap(~ gkz + year, FUN = fsum)  %>%
  mutate(einheit = "Bundesland")

load("Data/results/input/input_kommune_20221108.rdata")
# TODO: Solve Berlin issue

data_kommune <- createContextData(level = "kommune")

# before merging, make sure that Berlin context data is in the same format as results data
data_kommune[as.numeric(as.character(data_kommune$gkz_f))> 11000000 & as.numeric(as.character(data_kommune$gkz_f))<12000000, "gkz_f"] <- lapply(
       contextData_imputed[as.numeric(as.character(data_kommune$gkz_f))> 11000000 & as.numeric(as.character(data_kommune$gkz_f))<12000000, "gkz_f"], function (x)
       paste0(substr(as.character(x), start = 1, stop=5),"000"))

data_kommune <- data_kommune %>% filter(gkz_f %in% ind$gkz_f) %>%
  dplyr::rename(solar ="solare_strahlungsenergie") %>% mutate(kohle = braunkohle + steinkohle) %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))  %>%
  dplyr::rename(gkz =gkz_f) %>% dplyr::rename(bev =population) %>% mutate(year=2021) %>%
  filter(! (gkz %in% c("02000000", "11000000", "09162000"))) %>%
  mutate(einheit =case_when(substr(as.character(gkz), 1,2) %in% c("02", "11") ~ "Stadtbezirk",
                            substr(as.character(gkz), 1,5) == "09162" ~ "Stadtbezirk",
                            TRUE ~ "Kommune")) %>%
  select(gkz, year, bev,  kohle, wind, solar, einheit) %>% mutate(across(bev:solar, as.numeric))

#quantile(data_kommune2$bev)
 #0%     25%     50%     75%    100%
 #     0     623    1756    5544 1087214
votes_kommune <- read.csv("Data/contexts/votes/BTW 21/BTW 21 Ergebnisse/BTW 21 Ergebnisse Gemeinde/2021-bundestagswahl-gemeinden.csv") %>%
  clean_names() %>% dplyr::rename(gkz =ags) %>% dplyr::rename(gultige_zweitstimmen =ga_ltige) %>%
  dplyr::rename(cdu_csu =union)  %>% dplyr::rename(die_linke =linke)  %>% dplyr::rename(grune =gr_a_ne) %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>%
  mutate(cdu_csu = cdu_csu * gultige_zweitstimmen / 100)%>%
  mutate(spd = spd * gultige_zweitstimmen / 100)%>%
  mutate(die_linke = die_linke * gultige_zweitstimmen / 100)%>%
  mutate(af_d = af_d * gultige_zweitstimmen / 100)%>%
  mutate(fdp = fdp * gultige_zweitstimmen / 100)%>%
  mutate(grune = grune * gultige_zweitstimmen / 100)%>%
  select(gkz, gultige_zweitstimmen, cdu_csu, spd, die_linke, af_d, fdp, grune)

options(scipen = 999) # supress scientific notation
votes_kommune$gkz <- as.factor(ifelse(votes_kommune$gkz < 10000000, paste0("0", as.character(votes_kommune$gkz)),as.character(votes_kommune$gkz)))

votes_kommune$gkz <- as.factor(as.character(ifelse(as.numeric(as.character(votes_kommune$gkz)) > 17000000,
                                      paste0("0",substr(as.character(votes_kommune$gkz), 1,6), substr(as.character(votes_kommune$gkz), 8,8)),
                                      as.character(votes_kommune$gkz))))
votes_cities <- read.xlsx("Data/contexts/votes/BTW 21/BTW 21 Ergebnisse/BTW 21 Ergebnisse Gemeinde/kerg_edt.xlsx") %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>%
  clean_names() %>% mutate(cdu_csu = cdu + csu) %>% dplyr::rename(gultige_zweitstimmen =gueltige_zweitstimmen) %>%
  dplyr::rename(die_linke =linke) %>% dplyr::rename(af_d =afd) %>%
  select(gkz, gultige_zweitstimmen, cdu_csu, spd, die_linke, af_d, fdp, grune)

votes_kommune <- rbind(votes_kommune, votes_cities)

data_kommune <- left_join(data_kommune, votes_kommune)

data_all <- rbind(data_BL, data_LK, data_kommune)


### Einordnungen

data_all$partei1 <- colnames(data_all%>% select(af_d:fdp))[max.col(data_all%>% select(af_d:fdp))]
data_all <- data_all %>% mutate(ergebnis1 = round(pmax(af_d, grune, spd,cdu_csu,die_linke,fdp)/gultige_zweitstimmen*100)) %>%
  group_by(einheit) %>%
  mutate(bev2 = case_when(bev < as.numeric(quantile(bev, probs = 0.2)) ~ "klein",
                          bev > as.numeric(quantile(bev, probs = 0.8)) ~ "gross",
                          TRUE ~ "mittelgross")) %>%
  mutate(wind2 = case_when(wind > as.numeric(quantile(wind, probs = 0.7)) ~ "viel",TRUE ~ "")) %>%
  mutate(solar2 = case_when(solar > as.numeric(quantile(solar, probs = 0.7)) ~ "viel",TRUE ~ "")) %>%
  mutate(kohle2 = case_when(kohle > as.numeric(quantile(kohle, probs = 0.7)) ~ "viel",TRUE ~ ""))

data_all[data_all$einheit=="Kommune" & data_all$bev <40000, "bev2"] <- "mittelgross"

saveRDS(data_all, "Data/info_df_for_textelement.rds")
### Probleme:
# bev ist weird (welcher Ort hat Bev = 11??
# Zweitstimmen und % sind werid bei kommunen
# tec geht auch nicht, 0 ist doch nicht viel

summary(data_all$bev)
View(data_all[sample(nrow(data_all),10),])
data_all