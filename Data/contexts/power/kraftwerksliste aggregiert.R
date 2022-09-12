setwd("/Users/georg/Documents/Ariadne Work/Tabellen Regional /Kraftwerksliste/excel ")

NichtSol30 <- read.xlsx("Nicht Solar 30 -100.xlsx")
NichtSol100 <- read.xlsx("Nicht Solar 100 -500.xlsx")
NichtSol500 <- read.xlsx("Nicht Solar 500 - 900.xlsx")
NichtSol900 <- read.xlsx("Nicht solar 900 und mehr.xlsx")
Sol100 <- read.xlsx("Solar 1.xlsx")
Sol150 <- read.xlsx("Solar 2.xlsx")
Sol30 <- read.xlsx("Solar 30 - 32.xlsx")
Sol32 <- read.xlsx("Solar 32 -35.xlsx")
Sol35 <- read.xlsx("Solar 35 -41.xlsx")
Sol41 <- read.xlsx("Solar 41 - 51.xlsx")
Sol51 <- read.xlsx("Solar 51-70.xlsx")


Kraftwerke <- rbind.data.frame(NichtSol30, NichtSol100, NichtSol500, NichtSol900, 
                               Sol100, Sol150, Sol30, Sol32, Sol35, Sol41, Sol51)

 KWagg <- collap(Kraftwerke, Bruttoleistung.der.Einheit  ~ Gemeindeschlüssel + Energieträger, FUN=fsum)

 write.xlsx(KWagg, "Kraftwerke_aggregiert.xlsx") 
 write_rds(KWagg, "Kraftwerke_aggregiert.rds")
 