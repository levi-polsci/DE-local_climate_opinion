getCensusData <- function (level="landkreise", disaggregate_cities=FALSE){

  if(!(level %in% c("landkreise", "kommune")))
    stop("Invalid level selected. Currently implemented levels: landkreise")

  if(level=="landkreise")
  {
    #census <- read.xlsx("Data/Zensus/2000S-3054_Kreise.xlsx") # still uncleaned - use LMU provided census
    census <- readRDS("Data/census/census_2011_districts.rds") %>%
      dplyr::rename(lk_kz = kr_s, proportion=fraction)

    census$lk_kz <- as.factor(ifelse(census$lk_kz < 10000, paste0("0", as.character(census$lk_kz)),as.character(census$lk_kz)))
    #unique(dfImp[!(dfImp$lk_kz %in% census$lk_kz), "lk_kz"]) # check whther individual level landkreise are all in census -> YES

  }
  if(level=="kommune"){

    census <- read.xlsx("Data/census/2000S-3054_Gemeinden_formatted.xlsx") # read in kommune
    census_long <- pivot_longer(census, cols = '010010000000.Flensburg,.Stadt':'160770043043.Schmölln,.Stadt',
                                names_to = "id", values_to = "count") # Reshape to long
    census_long <- census_long %>% separate(id, into = c("gkz_f", "name"), extra = "merge", remove = FALSE) # split gkz_f and name
    summary(as.factor(substr(as.character(census_long$id),start=1,stop=2))) # check whether all Bundesländer are represented

    pop <- census_long[census_long$Bildung=="Insgesamt" &
                         census_long$Alter=="Insgesamt" & census_long$Geschlecht=="Insgesamt", c("id", "count")] #  attach gem_population
    colnames(pop)[2] <- "gem_population"
    pop$gem_population <- as.numeric(pop$gem_population)
    census_long <- left_join(census_long, pop)

    ### correct data gaps
    census_long$count <- str_replace(census_long$count,"/", "0") # The '/' sign indicates that values are available but too small, replace them for procedural reasons now with zero
    census_long$count <- str_replace(census_long$count,"-", "NA") # The '/' sign indicates that values are available but too small, replace them for procedural reasons now with zero
    census_long$count <- as.numeric(census_long$count)

    # i) Allocate excess totals to eduational categories
    educ <- census_long[census_long$Bildung!="Insgesamt" & census_long$Alter=="Insgesamt" & census_long$Geschlecht=="Insgesamt",]
    educ <- collap(educ, count ~ id, FUN = fsum)
    educ <- left_join(educ, pop)
    educ$excess <- educ$gem_population - educ$count
    educ$excess_share <- educ$excess /7
    ids <- census_long[census_long$Bildung!="Insgesamt" & census_long$Alter=="Insgesamt" &
                  census_long$Geschlecht=="Insgesamt", "id"]
    census_long[census_long$Bildung!="Insgesamt" & census_long$Alter=="Insgesamt" &
                  census_long$Geschlecht=="Insgesamt", "count"] <- census_long[census_long$Bildung!="Insgesamt" & census_long$Alter=="Insgesamt" &
                  census_long$Geschlecht=="Insgesamt", "count"] + as.numeric(unlist(lapply(ids$id, function (x) educ[educ$id==x, "excess_share"])))

    # ii) disaggregate total educations to age groups
    for (edu in pull(unique(census_long[census_long$Bildung!="Insgesamt", "Bildung"]), Bildung)){ # Run a loop for each educational level
      print(paste0("Correcting age distribution for education category '", edu,"' - calculating excess share..."))
      total <- census_long[census_long$Bildung==edu & census_long$Alter!="Insgesamt" & census_long$Geschlecht=="Insgesamt", c("id", "Alter", "count")]
      total <- collap(total, count ~ id, FUN = fsum) %>% dplyr::rename(sum=count)
      total <- left_join(total, census_long[census_long$Bildung==edu & census_long$Alter=="Insgesamt" & census_long$Geschlecht=="Insgesamt", c("id", "count")])
      total$excess <- total$count - total$sum # calculate number of respondents not assigned to each category
      total$excess_share <- as.numeric(unlist(lapply(total$id, function (x) (total[total$id==x, "excess"] /
        (as.numeric(nrow(census_long[census_long$id==x & census_long$Bildung==edu & census_long$Geschlecht=="Insgesamt" &  !is.na(census_long$count) & census_long$Alter!="Insgesamt", ]))-1))))) # Determine the number of cells which have non-zero respondents, confunsingly here coded as zero (sorry))
      print("... assigning topup ...")
      for (id in total$id){
        census_long[census_long$id==id & census_long$Bildung==edu & census_long$Geschlecht=="Insgesamt" &
                      !is.na(census_long$Alter) & census_long$Alter!="Insgesamt", "count"] <- census_long[census_long$id==id & census_long$Bildung==edu & census_long$Geschlecht=="Insgesamt" &
                      !is.na(census_long$Alter) & census_long$Alter!="Insgesamt", "count"] + as.numeric(total[total$id==id, "excess_share"]) # top-up all values with a relative share of the excess count

      }
    }
    print("Done: Correcting age distribution for education.")

    # iii) disaggregate gender similar procedure as above: determine excess & allocate
    # "... Abschluss einer Fachakademie oder Berufsakademie > 15 bis 17 Jahre ..."
    print("Starting to correct gender allocation based on educational and age groups...")
     for (edu in pull(unique(census_long[census_long$Bildung!="Insgesamt", "Bildung"]), Bildung)){ # Running outer loop for each educational level
       for (age in pull(unique(census_long[census_long$Alter!="Insgesamt", "Alter"]), Alter)){ # Running inner loop on age groups

         print(paste0("... ",edu," > ", age," ..."))
         total <- census_long[census_long$Bildung==edu & census_long$Alter==age & census_long$Geschlecht!="Insgesamt", c("id", "Geschlecht", "count")]
         if(nrow(total)!=0){
           total <- collap(total, count ~ id, FUN = fsum) %>% dplyr::rename(sum=count)
           total <- left_join(total, census_long[census_long$Bildung==edu & census_long$Alter==age & census_long$Geschlecht=="Insgesamt", c("id", "count")])
           total$excess <- total$count - total$sum # calculate number of respondents not assigned to each category
           ids_zero_all <- pull(census_long[census_long$Bildung==edu & census_long$Alter == age & census_long$Geschlecht=="Insgesamt" &
                                 is.na(census_long$count), ], id)# Locations where no one has this educational attainment
          #View(census_long[census_long$id=="010510044044.Heide,.Stadt",])
           #("010510044044.Heide,.Stadt" %in% ids_zero_all)
           id_zero_partial <- pull(census_long[!(census_long$id %in% ids_zero_all) & census_long$Bildung==edu & census_long$Alter == age &# census_long$Geschlecht!="Insgesamt" &
                                 is.na(census_long$count), ], id) # Locations where no only one gender has this educational attainment
           total[total$id %in% id_zero_partial,"excess_share"] <- total[total$id %in% id_zero_partial,"excess"] # where only one gender has this educational attainment, add the full topup to that gender
           total[!(total$id %in% id_zero_partial),"excess_share"] <- pull(total[!(total$id %in% id_zero_partial),], excess)/2 # for all others add half of the topup to each gender

           census_long[census_long$id==id & census_long$Bildung==edu & census_long$Geschlecht=="Insgesamt" &
                      !is.na(census_long$Alter) & census_long$Alter!="Insgesamt", "count"]

           #View(census_long[census_long$id=="010510044044.Heide,.Stadt",])
           total$excess_share <- as.numeric(unlist(lapply(total$id, function (x) (total[total$id==x, "excess"] / nrow(census_long[census_long$id==id & census_long$Bildung==edu & census_long$Alter==age &
                      !is.na(census_long$Geschlecht), "count"])-2)))) # Determine the number of cells which have non-zero respondents, confunsingly here coded as zero (sorry))
           print("... assigning topup ...")
           ids <- pull(census_long[census_long$Bildung==edu & census_long$Alter==age & census_long$Geschlecht!="Insgesamt" & !is.na(census_long$count), ],id)
           census_long[census_long$Bildung==edu & census_long$Alter==age &  census_long$Geschlecht!="Insgesamt"  & !is.na(census_long$count),
                     "count"] <- census_long[census_long$Bildung==edu & census_long$Alter==age & census_long$Geschlecht!="Insgesamt" & !is.na(census_long$count),
                                             "count"  ] + as.numeric(unlist(lapply(ids, function (x) pull(total[educ$id==x,], excess_share))))
         }
       }
     }
    print("Correction complete.")

    # re-transform NA to zero and remove insg.
    census_long[is.na(census_long$count), "count"] <- 0
    census <- census_long[census_long$Bildung!="Insgesamt" & census_long$Alter !="Insgesamt" & census_long$Geschlecht!="Insgesamt",]
    census <- census[order(census$id),]
    #census_gem <- census
    census <- census %>% dplyr::rename(educ_cat = Bildung, age_cat = Alter, gender=Geschlecht, gem_name=name) %>%
      mutate(proportion = count / gem_population) %>%
      mutate(across(c(educ_cat, age_cat, gender), ~ stringi::stri_trans_general(str = .x, id="de-ASCII; Latin-ASCII")))%>%
      mutate(educ_cat= dplyr::recode(educ_cat,
             `Ohne Abschluss` ="1",
             `Lehre, Berufsausbildung im dualen System`="2",
             `Fachschulabschluss`="3",
             `Abschluss einer Fachakademie oder Berufsakademie`="4",
             Fachhochschulabschluss = "5",
      `Hochschulabschluss`="6", Promotion="7")) %>%
      mutate(age_cat = dplyr::recode(age_cat,
             `15 bis 17 Jahre` ="NA",
             `18 bis 24 Jahre`="1",
             `25 bis 29 Jahre`="2",
             `30 bis 39 Jahre`="3",
             `40 bis 49 Jahre` = "4",
             `50 bis 64 Jahre`="5",
             `65 bis 74 Jahre`='6',
             `75 Jahre und aelter`="7")) %>%
      mutate(gender= dplyr::recode(gender, `Maennlich`='1', Weiblich='0')) %>%
      mutate(across(c(educ_cat, age_cat, gender), as.factor)) %>%
      filter(age_cat!="NA") %>%
      select(gkz_f, gem_name, age_cat, gender , educ_cat, gem_population, count, proportion)

      if(disaggregate_cities)
    census <- census %>% scaleBerlinCensus() %>%
      scaleHamburgCensus() %>% scaleMunichCensus()

  census$gkz_f <- paste0(substr(as.character(census$gkz_f), start = 1, stop=5),substr(as.character(census$gkz_f), start = 10, stop=12))
  }

  # correct issues: ex
  census[is.infinite(census$proportion), c("count","proportion")] <- 0
  census[census$proportion<0, c("count","proportion")] <- 0


  return(census)

}