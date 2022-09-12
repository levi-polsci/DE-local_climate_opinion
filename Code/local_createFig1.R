###### Create figure with six maps

# -- obtain list of all files in the folder
filenames_in <- list.files("Data/results/output/Landkreis")
# -- select variables
filenames_in <- filenames_in[c(2, # bike 2021
                               3, # builduing levy 2021
                               9, # coal exit 2019
                               23, # gas furnace 2021
                               30, # ice ban 2021
                               #50, # tax rebate insulation landlord 2021
                               11 # ctax 2019
)]

# -- obtain list of variables, years, & German and English subtitles legend titles
ls_vars <- sub("*fittedModel_y.","", sub("-.*", "",filenames_in))
ls_years <- sub(".*-","", sub(".rds.*", "",filenames_in))
ttl_map <- read.xlsx("Data/title_data.xlsx")

ls_subtitles_EN <- unlist(lapply(ls_vars, function (x) ttl_map[ttl_map$var==x,"subtitle_EN"]))
ls_legendtlt_EN <- unlist(lapply(ls_vars, function (x) ttl_map[ttl_map$var==x,"legend_EN"]))
ls_subtitles_EN <- gsub("xxx", "\n", ls_subtitles_EN)


# -- prepare map object
map_root <- st_read("Data/shapefiles/gadm40_DEU_2.shp") # get spatial data for Germany at the county level
map_root <- st_transform(map_root, crs = 'ESRI:31494')
obj_lst <- list()

 Sys.setlocale("LC_CTYPE", "german")
lgd_ttl <- "Anteil der Bev\366lkerung, der \nMaßnahme zumindest teilweise unterst\374tzt"
lgd_ttl <- "Share of population that supports \npolicy at least partly"


plot_data <- function (i){

  load(paste0("Data/results/output/Landkreis/",filenames_in[i]))
  #data <- readRDS(paste0("Data/results/",filenames_in[i])) # -- load data
  data <- ml_model

  predictions <- data$ebma %>% dplyr::rename(CC_2=lk_kz, var=ebma) # -- join map with data
  predictions$CC_2 <- as.character(predictions$CC_2)
  predictions[predictions$CC_2=="03159", "CC_2"]<-"03152" # correct lk_kz for Göttingen
  predictions <- rbind(predictions, predictions %>% filter(CC_2=="03152") %>%mutate(CC_2="03156")) # Add record for Osterrode which was formerly merged with Göttigngen
  map <- left_join(map_root, predictions)

  print(filenames_in[i])

  ggplot(data = map) +
      geom_sf(aes(fill = var), lwd=0) +   #labs(subtitle = paste0(ls_subtitles[i], ", in ", ls_years[i])) +
      scale_fill_gradientn(colors = c("#00006c","#8e7ea0", "#e3cb42", "#c36c17", "#8b0000"), na.value = "grey80",
                           limits=c(0,1), labels = scales::percent, name = lgd_ttl)+
      theme_void()+ theme(    #legend.justification = c("left", "top"),
    legend.position ="bottom",plot.margin = unit(c(0, 0, 0, 0), "cm"),
    text = element_text(family = "Tahoma",  size = 10), plot.subtitle = element_text(family = "Tahoma", size = 10),legend.title = element_text(family = "Tahoma", size = 10),
    legend.text = element_text(family = "Tahoma", size = 10),
    legend.key.size = unit(1, 'cm'))
}
myplots <- lapply(1:6, plot_data)

# https://stackoverflow.com/questions/28984775/r-barplot-with-german-encoding
# \344 ä
# \366 ö
# \374 ü
# \337 ß
plots <- ggarrange(myplots[[1]], myplots[[2]],myplots[[3]], myplots[[4]],myplots[[5]], myplots[[6]],
         # labels = c("a) Ausbau Fahrradwege", "b) Klima-Geb\344udesteuer", "c) Kohleausstieg",
          #           "d) Neuzulassungsstop Gasheizungen", "e) Neuzul.-stop Verbrenner-PKW", "f) H\366here CO2-Bepreisung"),
                   labels = c("a) Expansion of bikelanes", "b) Buildings  levy", "c) Coal exit",
                     "d) Ban of new gas heating", "e) Phase out of fossil cars", "f) Higher CO2 Pricing"),
          ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom", font.label = list(size = 8, face="bold"), hjust =0)
plots
ggsave("Figures/Fig 1 - Maps_EN.png", width = 1800, height = 2000 , units = "px")

#######################################################
### descriptive analysis
#######################################################


### subset & transform data

context <- readRDS("Data/contexts/other - landkreise/Context 2017-2019+2021/Kontextdaten2016-2019+2021.rds") %>% clean_names() %>%
  dplyr::rename(ags ="gkz") %>% dplyr::rename(solar ="solare_strahlungsenergie") %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))  %>%  select(ags, year, bev, arbeitslose_insgesamt,
  braunkohle, wind, steinkohle, solar,af_d, grune,cdu_csu,fdp)

data <- readRDS("Data/results/results_lk_20220720.rds") %>% select(ags, Landkreisname, Bundesland, year, var, value) %>% left_join(context)
data$bev <- as.numeric(data$bev)
# attach context variables

# large cities
mean(pull(data[data$var=="ctax" & data$year=="2019",], "value"), na.rm=TRUE)
mean(pull(data[data$var=="ctax" & data$year=="2019" &
            (grepl("Stadt", data$Landkreisname) |
            grepl("stadt", data$Landkreisname)) &
  data$bev>500000,], "value"), na.rm=TRUE)

mean(pull(data[data$var=="ctax" & data$year=="2019" &
                 !((grepl("Stadt", data$Landkreisname) |
            grepl("stadt", data$Landkreisname)) &
  data$bev>500000),], "value"), na.rm=TRUE)

mean(pull(data[data$var=="ctax" & data$year=="2019" &
            (grepl("Stadt", data$Landkreisname) |
            grepl("stadt", data$Landkreisname)) &
  data$bev>250000,], "value"), na.rm=TRUE)

# bike
data[data$var=="bike" & data$Bundesland=="Berlin","value"]
data[data$var=="bike" & data$Bundesland=="Hamburg","value"]
mean(pull(data[data$var=="bike",], "value"), na.rm=TRUE)

# East/West
mean(pull(data[data$var=="ctax" & data$Bundesland %in% c("Brandenburg", "Thüringen","Mecklenburg-Vorpommern","Sachsen", "Sachsen-Anhalt"),], "value"), na.rm=TRUE)
mean(pull(data[data$var=="ctax" & !(data$Bundesland %in% c("Brandenburg", "Thüringen","Mecklenburg-Vorpommern","Sachsen", "Sachsen-Anhalt")),], "value"), na.rm=TRUE)


