#### Wegweiser Kommunen Daten

setwd("")
library(openxlsx)

## Daten für 2016

Wärme2016 <- read.xlsx("Wäremdaten_WegWei_2016.xlsx")
Wärme2016$year <- 2016


## Daten für 2017

Wärme2017 <- read.xlsx("Wäremdaten_WegWei_2017.xlsx")
Wärme2017$year <- 2017

## Daten für 2018

Wärme2018 <- read.xlsx("Wäremdaten_WegWei_2018.xlsx")
Wärme2018$year <- 2018

## Daten für 2019

Wärme2019 <- read.xlsx("Wäremdaten_WegWei_2019.xlsx")
Wärme2019$year <- 2019

##Zensus Heizunsdaten

GebäudeHeizung <- read.xlsx("GebäudeHeizung_Zensus.xlsx")


## Combine Data

Wärme <- rbind.data.frame(Wärme2016, Wärme2017, Wärme2018, Wärme2019)

## Export Excel

write.xlsx(Wärme, "Wärmedaten.xlsx")