library(openxlsx)

# Open
wirtschaftsabschnitte <- read.xlsx("Data/52111-04-01-4.xlsx", startRow = 5)
# Rename columns
colnames(wirtschaftsabschnitte) <- c("gkz", "gemeinde_name", "WirtschAbsGesamt", "WirtschAbsBergbau","WirtschAbsVerarbeitend", "WirtschAbsEnergie","WirtschAbsWasser", "WirtschAbsBau", "WirtschAbsHandel",
                                     "WirtschAbsVerkehr", "WirtschAbsGatro", "WirtschAbsInfo", "WirtschAbsFinanz", "WirtschAbsWohn", "WirtschAbsWissen", "WirtschAbsWirtschaftsservce",
                                     "WirtschAbsBildung", "WirtschAbsSozial", "WirtschAbsKultur", "WirtschAbsService")
View(head(wirtschaftsabschnitte))
# Assign years
wirtschaftsabschnitte$Year <-0
wirtschaftsabschnitte$id <- rownames(wirtschaftsabschnitte)
wirtschaftsabschnitte[as.numeric(wirtschaftsabschnitte$id)>as.numeric(wirtschaftsabschnitte[!is.na(wirtschaftsabschnitte$gkz) & wirtschaftsabschnitte$gkz=="2018","id"]) &
   as.numeric(wirtschaftsabschnitte$id)<as.numeric(wirtschaftsabschnitte[!is.na(wirtschaftsabschnitte$gkz) & wirtschaftsabschnitte$gkz=="2017","id"]) ,"Year"]<- 2018
wirtschaftsabschnitte[as.numeric(wirtschaftsabschnitte$id)>as.numeric(wirtschaftsabschnitte[!is.na(wirtschaftsabschnitte$gkz) & wirtschaftsabschnitte$gkz=="2017","id"]) &
   as.numeric(wirtschaftsabschnitte$id)<as.numeric(wirtschaftsabschnitte[!is.na(wirtschaftsabschnitte$gkz) & wirtschaftsabschnitte$gkz=="2016","id"]) ,"Year"]<- 2017
wirtschaftsabschnitte[as.numeric(wirtschaftsabschnitte$id)>as.numeric(wirtschaftsabschnitte[!is.na(wirtschaftsabschnitte$gkz) & wirtschaftsabschnitte$gkz=="2017","id"]),"Year"]<- 2016

wirtschaftsabschnitte <- wirtschaftsabschnitte %>%
  drop_na(gemeinde_name) %>%
  relocate(Year, .before=WirtschAbsGesamt) %>%
  select(-'id')
