#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compose figure 2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# I. Data collection
#####################

###### Create figure with four variables over four points of time

# -- obtain list of all files in the folder
filenames_in <- list.files("Data/results/output/Landkreis")
# -- select variables
filenames_in <- filenames_in[c(5:7, # coal exit
                               25:27, # ice ban
                               38:41, #, # Onshore
                               50:52)] # solarplant

# -- obtain list of variables, years, & German and English subtitles legend titles
ls_vars <- sub("*fittedModel_y.","", sub("-.*", "",filenames_in))
ls_years <- sub(".*-","", sub(".rds.*", "",filenames_in))


# -- prepare map object
map_root <- st_read("Data/shapefiles/gadm40_DEU_2.shp") # get spatial data for Germany at the county level
map_root <- st_transform(map_root, crs = 'ESRI:31494')
obj_lst <- list()

 Sys.setlocale("LC_CTYPE", "german")
lgd_ttl <- "Anteil der \nBev\366lkerung,\nder Maßnahme \nzumindest teilweise\nunterst\374tzt"
# https://stackoverflow.com/questions/28984775/r-barplot-with-german-encoding
# \344 ä
# \366 ö
# \374 ü
# \337 ß

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

  ttl <- ""
  if(i ==7)
    ttl <- "2017"
  if(i ==8)
    ttl <- "2018"
  if(i ==9)
    ttl <- "2019"
  if(i ==10)
    ttl <- "2021"

  subttl <- ""
  if(i==7)
    subttl <- "a) Ausbau von Windenergieanlagen an Land" #"a) Expansion of onshore wind"
  if(i==11)
    subttl <- "b) Ausbau von Freifl\344chensolaranlagen"#"b) Expansion of solar parks"
  if(i==1)
    subttl <- "c) Ausstieg aus der Kohleverstromung"#"c) Termination of coal combustion"
  if(i==4)
    subttl <- "d) Neuzulassungsstop Verbrenner-PKW bis 2030"#"d) Phase out of fossil cars from 2030"

  ggplot(data = map) +
      geom_sf(aes(fill = var), lwd=0) +   labs(title = ttl, subtitle = subttl) +
      scale_fill_gradientn(colors = c("#00006c","#8e7ea0", "#e3cb42", "#c36c17", "#8b0000"), na.value = "grey80",
                           limits=c(0,1), labels = scales::percent, name = lgd_ttl)+
      theme_void()+ theme(    #legend.justification = c("left", "top"),
    text = element_text(family = "Tahoma",  size = 8),
    legend.position = "none",
    plot.subtitle = element_text(family = "Tahoma", size = 11),
    plot.title = element_text(family = "Tahoma", size = 13, hjust=0.5),
    legend.title = element_text(family = "Tahoma", size = 13),
    legend.text = element_text(family = "Tahoma", size = 13),
    legend.key.size = unit(1, 'cm'))
}
yr_plots <- lapply(seq_along(filenames_in), plot_data)

# II. Print
#####################

legend <- get_legend(yr_plots[[1]]+theme(legend.position ="right",plot.margin = unit(c(0.0, 0, 0, 0), "cm"),
                                         legend.key.size = unit(1, 'cm'), legend.text = element_text(family = "Tahoma", size = 13)))


#pdf(file = "Figures/Fig 2 - change over time.png", )
p <- grid.arrange(yr_plots[[7]], yr_plots[[8]],yr_plots[[9]], yr_plots[[10]], # onshore
                   yr_plots[[11]], yr_plots[[12]],yr_plots[[13]], # solarplant
                   yr_plots[[1]], yr_plots[[2]],yr_plots[[3]], # coal exit
                   yr_plots[[4]], yr_plots[[5]],yr_plots[[6]],
                                           legend,# Ice ban
          #labels = c("a", "", "", "", "b", "", "","c", "", "",  "d", "", ""),
          #ncol = 4, nrow = 4,
          #common.legend = TRUE, 
          #legend = "right",
          layout_matrix=rbind(c(1,2,3,4,14),
                                        c(NA,5,6,7,14),
                                       c(8,9, 10,NA,14),
                                       c(NA, 11,12,13,14)#,
                              #c(14,14,14,14))
))



#dev.off()
ggsave(plot=p, filename="Figures/Fig 2 - change over time_DE.png", width = 3000, height = 3600 , units = "px")


# IV. Descriptive analysis
#####################
